
(*> goto put somewhere common - is used in Output too*)
let ns_of_sec n = Int64.of_float (1e9 *. n)
let sec_of_ns ns = Int64.to_float ns /. 1e9

let sleep_ns_before_retry = ns_of_sec 1.

type read = [
  | `Eof
  | `Data of Cstruct.t
]

(*> Note create with e.g.:
  let err = Fmt.str "%a" S.TCP.pp_error err in
*)
type 'a error = [> `Msg of string ] as 'a


module type FLOW = sig

  type t

  val listen : port:int -> (t -> unit Lwt.t) -> unit
  val unlisten : port:int -> unit

  val create_connection :
    id:string -> (Ipaddr.t * int) -> (t, _ error) Lwt_result.t
  
  val read : t -> (read, _ error) Lwt_result.t
  val writev : t -> index:int -> Cstruct.t list -> (unit, _ error) Lwt_result.t
  val close : t -> unit Lwt.t

  val dst : t -> Ipaddr.t * int
  
end

module type INFO = sig

  val subproto : Types.protocol

end

module Make
    (Time : Mirage_time.S)
    (Flow : FLOW)
    (O : Output.S)
    (Info : INFO)
= struct

  open Lwt.Infix 
  open Lwt_result.Syntax
  
  module Listen = struct

    type context = {
      flow : Flow.t;
      packet_index : int;
      dst : Ipaddr.t;
      dst_port : int;
      conn_id : string;
      conn_id_client : string option;
      progress : unit -> unit Lwt.t;
    }

    let pier_of_ctx ctx = Types.Pier.{
      protocol = Info.subproto;
      ip = ctx.dst;
      port = ctx.dst_port;
      conn_id = ctx.conn_id;
    }

    let header_of_ctx ctx =
      ctx.conn_id_client |> Option.map (fun connection_id ->
        Packet.T.{
          index = ctx.packet_index;
          connection_id;
          meta = `Normal;
        }
      )
      |> Option.to_result
        ~none:(`Msg "header_of_ctx: Failure: ctx.conn_id_client is None")

    let start ~name ~port ~timeout =
      let module O = O.Listen in
      let rec read_packet
          ~ctx ?more_data ?(ignore_data=false) ?unfinished_packet () =
        let data = Option.value more_data ~default:Cstruct.empty in
        let* unfinished =
          match unfinished_packet with
          | None -> Packet.Tcp.init ~ignore_data data |> Lwt.return
          | Some unfinished ->
            Packet.Tcp.append ~ignore_data ~data unfinished |> Lwt.return
        in
        match unfinished with
        | `Unfinished unfinished_packet ->
          let* res = Flow.read ctx.flow in
          begin match res with 
            | `Eof -> Lwt_result.fail @@ `Msg "Server closed connection"
            | `Data more_data ->
              ctx.progress () >>= fun () ->
              read_packet ~ctx ~more_data ~unfinished_packet ~ignore_data ()
          end
        | `Done v -> Lwt_result.return v
      and read_n_packets_ignoring_data ~ctx ~n ~more_data =
        let pier = pier_of_ctx ctx in
        let ignore_data = true in
        let rec aux ?more_data n =
          if n <= 0 then Lwt_result.return more_data else
            let* packet, more_data =
              read_packet ~ctx ~ignore_data ?more_data ()
            in
            let header = packet.Packet.T.header in
            let protocol = None in
            O.received_packet ~pier ~header ~protocol;
            aux ?more_data (pred n)
        in
        aux n ?more_data
      and handle_packet ~ctx ~packet ~more_data =
        let pier = pier_of_ctx ctx in
        let header = packet.Packet.T.header in
        let ctx = { ctx with conn_id_client = Some header.connection_id } in
        let* protocol = packet.data |> Protocol_msg.of_cstruct |> Lwt.return in
        match protocol with
        | `Hello hello ->
          let protocol = Some protocol in
          O.received_packet ~pier ~header ~protocol;
          let protocol = `Hello Protocol_msg.T.{ name } in
          let* ctx = respond ~ctx ~protocol in
          let* packet, more_data = read_packet ~ctx ?more_data () in
          handle_packet ~ctx ~packet ~more_data
        | `Bandwidth bwm ->
          begin match bwm.Protocol_msg.T.direction with
            | `Up ->
              let protocol = Some protocol in
              O.received_packet ~pier ~header ~protocol;
              let* more_data =
                read_n_packets_ignoring_data
                  ~ctx
                  ~n:bwm.Protocol_msg.T.n_packets
                  ~more_data
              in
              let* packet, more_data = read_packet ~ctx ?more_data () in
              handle_packet ~ctx ~packet ~more_data
            | `Down -> 
              let protocol = Some protocol in
              O.received_packet ~pier ~header ~protocol;
              let n = bwm.Protocol_msg.T.n_packets in
              let data =
                String.make bwm.packet_size '%'
                |> Cstruct.of_string
              in
              let* ctx = respond_with_n_copies ~ctx ~n ~data in
              let* packet, more_data = read_packet ~ctx ?more_data () in
              handle_packet ~ctx ~packet ~more_data
          end
        | `Latency `Ping ->
          let protocol = Some protocol in
          O.received_packet ~pier ~header ~protocol;
          let header = packet.header in
          let protocol = `Latency `Pong in
          let* ctx = respond ~ctx ~protocol in
          let* packet, more_data = read_packet ~ctx ?more_data () in
          handle_packet ~ctx ~packet ~more_data
        | `Latency `Pong -> 
          let protocol = Some protocol in
          O.received_packet ~pier ~header ~protocol;
          let* packet, more_data = read_packet ~ctx ?more_data () in
          handle_packet ~ctx ~packet ~more_data
      and respond ~ctx ~protocol =
        let* header = header_of_ctx ctx |> Lwt.return in 
        let pier = pier_of_ctx ctx in
        let data = protocol |> Protocol_msg.to_cstruct in
        let response = Packet.to_cstructs ~header ~data in
        let* () = Flow.writev ctx.flow response ~index:header.index in
        ctx.progress () >>= fun () ->
        let protocol = Some protocol in
        O.sent_packet ~pier ~header ~protocol;
        let ctx = { ctx with packet_index = succ ctx.packet_index } in
        Lwt_result.return ctx
      and respond_with_n_copies ~ctx ~n ~data =
        let* header = header_of_ctx ctx |> Lwt.return in 
        let pier = pier_of_ctx ctx in
        if n <= 0 then Lwt_result.return ctx else 
          let { flow; dst; dst_port; conn_id } = ctx in
          let response = Packet.to_cstructs ~header ~data in
          let* () = Flow.writev flow response ~index:header.index in
          ctx.progress () >>= fun () ->
          let protocol = None in
          O.sent_packet ~pier ~header ~protocol;
          let ctx = { ctx with packet_index = succ ctx.packet_index } in
          let n = pred n in
          respond_with_n_copies ~ctx ~n ~data 
      in
      let callback flow =
        Mirage_runtime.at_exit (fun () -> Flow.close flow);
        let dst, dst_port = Flow.dst flow in
        let conn_id = Uuidm.(v `V4 |> to_string) in
        let conn_id_client = None in
        let sleep_ns = Time.sleep_ns in
        let timeout_ns = ns_of_sec (float timeout) in
        let timeout_state = Timeout.make ~sleep_ns ~timeout_ns in
        let progress () = Timeout.progress timeout_state in
        let ctx = {
          flow;
          packet_index = 0;
          dst;
          dst_port;
          conn_id;
          conn_id_client;
          progress;
        }
        in
        let pier = pier_of_ctx ctx in
        Lwt.catch
          (fun () ->
              O.new_connection ~pier;
              let handle_t =
                let* packet, more_data = read_packet ~ctx () in 
                handle_packet ~ctx ~packet ~more_data
              in
              Timeout.cancel_on_timeout timeout_state handle_t;
              handle_t >>= function
              | Ok () ->
                O.closing_connection ~pier;
                Flow.close flow
              | Error `Msg err ->
                O.error ~pier ~err;
                O.closing_connection ~pier;
                Flow.close flow
          )
          (fun exn -> 
              let err = Printexc.to_string exn in
              O.error ~pier ~err;
              O.closing_connection ~pier;
              Flow.close flow
          )
      in
      Mirage_runtime.at_exit (fun () ->
        Flow.unlisten ~port |> Lwt.return
      );
      Flow.listen ~port callback;
      O.registered_listener ~proto:Info.subproto ~port

  end

  module Connect = struct

    type context = {
      flow : Flow.t;
      packet_index : int;
      conn_id : string;
      progress : unit -> unit Lwt.t;
    }

    let make_pier ~conn_id ~ip ~port = Types.Pier.{
      protocol = Info.subproto;
      ip;
      port;
      conn_id;
    }
    
    let sleep_ns_before_retry = ns_of_sec 1.

    let start ~name ~port ~ip ~monitor_bandwidth ~timeout =
      let open Lwt_result.Syntax in
      let module O = O.Connect in
      let bandwidth_testdata_str =
        String.make monitor_bandwidth#packet_size '%' in
      let bandwidth_testdata = Cstruct.of_string bandwidth_testdata_str in
      let n_bandwidth_packets =
        2000. *. 128e3 /. float monitor_bandwidth#packet_size |> truncate
      in
      let conn_id = Uuidm.(v `V4 |> to_string) in
      let pier = make_pier ~conn_id ~ip ~port 
      in
      let rec loop_try_connect () =
        O.connecting ~pier;
        Flow.create_connection ~id:conn_id (ip, port) >>= function
        | Error (`Msg err) ->
          O.error_connection ~pier ~err;
          Time.sleep_ns sleep_ns_before_retry >>= fun () ->
          loop_try_connect ()
        | Ok flow ->
          O.connected ~pier;
          Mirage_runtime.at_exit (fun () -> Flow.close flow);
          let sleep_ns = Time.sleep_ns in
          let timeout_ns = ns_of_sec (float timeout) in
          let timeout_state = Timeout.make ~sleep_ns ~timeout_ns in
          let progress () = Timeout.progress timeout_state in
          let ctx = {
            flow;
            conn_id;
            packet_index = 0;
            progress;
          } in
          let handle_t =
            Lwt.catch
              (fun () ->
                  let t = write_more ~ctx ~conn_state:`Init in
                  Timeout.cancel_on_timeout timeout_state t;
                  t
              )
              (fun exn -> Lwt_result.fail @@ `Msg (Printexc.to_string exn))
          in
          handle_t >>= function
          | Ok _ ->
            O.closing_flow ~pier;
            Flow.close flow >>= fun () ->
            O.closed_flow ~pier;
            Time.sleep_ns sleep_ns_before_retry >>= fun () ->
            loop_try_connect ()
          | Error err ->
            O.error ~pier ~err;
            O.closing_flow ~pier;
            Flow.close flow >>= fun () ->
            O.closed_flow ~pier;
            Time.sleep_ns sleep_ns_before_retry >>= fun () ->
            loop_try_connect ()
      and write_more ~ctx ~conn_state =
        let header = Packet.T.{
          index = ctx.packet_index;
          connection_id = ctx.conn_id;
          meta = `Normal;
        } in
        match conn_state with
        | `Init ->
          let protocol = `Hello Protocol_msg.T.{ name } in
          let* ctx = write_packet ~ctx ~header ~protocol in
          let* _more_data = read_packet ~ctx () in
          let conn_state = `Latency in
          write_more ~ctx ~conn_state
        | `Latency ->
          let protocol = `Latency `Ping in
          let* ctx = write_packet ~ctx ~header ~protocol in
          (*> Note: optimistically expecting `Latency `Pong back*)
          let* _more_data = read_packet ~ctx () in
          let header = Packet.T.{
            index = ctx.packet_index;
            connection_id = ctx.conn_id;
            meta = `Normal;
          } in
          let protocol = `Latency `Pong in
          let* ctx = write_packet ~ctx ~header ~protocol in
          if monitor_bandwidth#enabled then 
            let conn_state = `Bandwidth `Up in
            write_more ~ctx ~conn_state
          else
            let conn_state = `Latency in
            Time.sleep_ns @@ ns_of_sec (1. /. 2.) >>= fun () ->
            write_more ~ctx ~conn_state
        | `Bandwidth (`Up as direction) ->
          let protocol = `Bandwidth Protocol_msg.T.{
            direction;
            n_packets = n_bandwidth_packets;
            (*< goto control via CLI - implicitly via 'test-size'*)
            packet_size = monitor_bandwidth#packet_size;
          } in 
          let* ctx = write_packet ~ctx ~header ~protocol in
          let* ctx =
            write_n_copies
              ~ctx
              ~n:n_bandwidth_packets
              ~data:bandwidth_testdata
          in
          let conn_state = `Bandwidth `Down in
          write_more ~ctx ~conn_state
        | `Bandwidth (`Down as direction) ->
          let protocol = `Bandwidth Protocol_msg.T.{
            direction;
            n_packets = n_bandwidth_packets; 
            packet_size = monitor_bandwidth#packet_size;
          } in 
          let* ctx = write_packet ~ctx ~header ~protocol in
          let* () = read_n_packets_ignoring_data ~ctx ~n:n_bandwidth_packets in
          let conn_state = `Latency in
          write_more ~ctx ~conn_state
      and write_packet ~ctx ~header ~protocol =
        let data = Protocol_msg.to_cstruct protocol in
        let* () = 
          Packet.to_cstructs ~header ~data
          |> Flow.writev ctx.flow ~index:header.index
        in
        ctx.progress () >>= fun () ->
        let protocol = Some protocol in
        O.sent_packet ~pier ~header ~protocol;
        let ctx =
          let packet_index = succ ctx.packet_index in
          { ctx with packet_index }
        in
        Lwt_result.return ctx
      and write_n_copies ~ctx ~n ~data =
        let connection_id = ctx.conn_id
        in
        let rec loop ~packet_index n =
          if n <= 0 then
            Lwt_result.return packet_index
          else 
            let header = Packet.T.{
              index = packet_index;
              connection_id;
              meta = `Normal;
            }
            in
            let data = Packet.to_cstructs ~header ~data in
            let* () = Flow.writev ctx.flow data ~index:header.index in
            ctx.progress () >>= fun () ->
            let protocol = None in
            O.sent_packet ~pier ~header ~protocol;
            loop ~packet_index:(succ packet_index) (pred n)
        in
        let+ packet_index = loop ~packet_index:ctx.packet_index n in
        { ctx with packet_index }
      and read_n_packets_ignoring_data ~ctx ~n =
        let rec aux ?more_data n =
          if n <= 0 then Lwt_result.return () else
            let* more_data =
              read_packet
                ~ctx
                ~ignore_protocol:true
                ?more_data
                ()
            in
            aux ?more_data (pred n)
        in
        aux n
      and read_packet
          ~ctx
          ?more_data
          ?unfinished_packet
          ?(ignore_protocol=false)
          ()
        =
        (*> goto add this? *)
        (* O.reading_response ~ip ~port; *)
        let input_t =
          match more_data with
          | Some more_data ->
            Lwt_result.return @@ `Data more_data
          | None ->
            Flow.read ctx.flow
        in
        let* input_res = input_t in
        match input_res with
        | `Eof -> Lwt_result.fail `Eof
        | `Data data ->
          ctx.progress () >>= fun () ->
          let ignore_data = ignore_protocol in
          let* unfinished = match unfinished_packet with
            | None ->
              Packet.Tcp.init ~ignore_data data |> Lwt.return
            | Some unfinished ->
              Packet.Tcp.append ~data ~ignore_data unfinished |> Lwt.return
          in
          begin match unfinished with
            | `Done (packet, more_data) ->
              let header = packet.Packet.T.header in
              let+ protocol =
                if ignore_protocol then Lwt_result.return None else
                  let+ protocol =
                    packet.Packet.T.data
                    |> Protocol_msg.of_cstruct
                    |> Lwt.return
                  in
                  Some protocol
              in
              (*> gomaybe fail on wrong packet recived? - optimistic makes sense,
                  .. as there can be so many edgecases that I don't want to catch
                     * < this would need some 'expected' param, or returning header * protocol
              *)
              O.received_packet ~pier ~header ~protocol;
              more_data
            | `Unfinished packet ->
              read_packet ~ctx ~unfinished_packet:packet ~ignore_protocol ()
          end
      in
      loop_try_connect ()

  end


end


