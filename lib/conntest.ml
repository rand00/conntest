open Lwt.Infix

module Output = Output

let (let*) = Result.bind 
let (let+) x f = Result.map f x 

(*> goto put somewhere common - is used in Output too*)
let ns_of_sec n = Int64.of_float (1e9 *. n)
let sec_of_ns ns = Int64.to_float ns /. 1e9

module type S = sig

  type stack
  type udp_error
  
  module Listen : sig

    val tcp : name:string -> port:int -> timeout:int -> unit
    val udp : name:string -> port:int -> timeout:int -> unit

  end

  module Connect : sig

    val tcp :
      name:string ->
      port:int ->
      ip:Ipaddr.t ->
      monitor_bandwidth:< enabled : bool; packet_size : int; .. > ->
      timeout:int ->
      'a Lwt.t

    val udp :
      name:string ->
      port:int ->
      ip:Ipaddr.t ->
      monitor_bandwidth:'a ->
      timeout:int ->
      unit Lwt.t

  end

end

module type STACK_V = sig
  type t 
  val stack : t
end

module Make
    (Time : Mirage_time.S)
    (S : Tcpip.Stack.V4V6)
    (Sv : STACK_V with type t = S.t)
    (O : Output.S)
= struct

  type stack = S.t
  type udp_error = S.UDP.error

  module Timeout = struct

    type t = {
      timeout_ns : Int64.t;
      progress : unit Lwt_mvar.t;
      timeout : unit Lwt.t;
    }

    let make ~timeout_ns =
      let progress = Lwt_mvar.create () in
      let rec return_on_timeout () =
        Lwt.pick [
          (Lwt_mvar.take progress >|= fun () -> `Progress);
          (Time.sleep_ns timeout_ns >|= fun () -> `Timeout);
        ]
        >>= function
        | `Progress -> return_on_timeout ()
        | `Timeout -> Lwt.return_unit
      in
      let timeout = return_on_timeout () in
      { timeout_ns; progress; timeout }

    let progress : t -> unit Lwt.t = fun state ->
      Lwt_mvar.put state.progress ()

    let cancel_on_timeout : t -> 'a Lwt.t -> unit =
      fun state t ->
      Lwt.async (fun () -> state.timeout >|= fun () -> Lwt.cancel t)

    let on_timeout : t -> (unit -> unit Lwt.t) -> unit Lwt.t =
      fun state f ->
      state.timeout >>= f
    
  end

  module Listen = struct

    type context = {
      flow : S.TCP.flow;
      dst : Ipaddr.t;
      dst_port : int;
      conn_id : string;
      progress : unit -> unit Lwt.t;
    }

    let tcp ~name ~port ~timeout =
      let module O = O.Listen.Tcp in
      let open Lwt_result.Syntax
      in
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
          begin
            S.TCP.read ctx.flow >>= function
            | Ok `Eof -> Lwt_result.fail @@ `Msg "Server closed connection"
            | Ok (`Data more_data) ->
              ctx.progress () >>= fun () ->
              read_packet ~ctx ~more_data ~unfinished_packet ~ignore_data ()
            | Error e ->
              let msg = Fmt.str "%a" S.TCP.pp_error e in
              Lwt_result.fail @@ `Msg msg
          end
        | `Done v -> Lwt_result.return v
      and read_n_packets_ignoring_data ~ctx ~n ~more_data =
        let { conn_id; dst; dst_port; _ } = ctx in
        let ignore_data = true in
        let rec aux ?more_data n =
          if n <= 0 then Lwt_result.return more_data else
            let* packet, more_data =
              read_packet ~ctx ~ignore_data ?more_data ()
            in
            let header = packet.Packet.T.header in
            let protocol = None in
            O.received_packet ~conn_id ~ip:dst ~port:dst_port ~header ~protocol;
            aux ?more_data (pred n)
        in
        aux n ?more_data
      and handle_packet ~ctx ~packet ~more_data =
        let { conn_id; dst; dst_port; _ } = ctx in
        let header = packet.Packet.T.header in
        let* protocol = packet.data |> Protocol.of_cstruct |> Lwt.return in
        match protocol with
        | `Hello hello ->
          let protocol = Some protocol in
          O.received_packet ~conn_id ~ip:dst ~port:dst_port
            ~header ~protocol;
          let protocol = `Hello Protocol.T.{ name } in
          let* () = respond ~ctx ~header ~protocol in
          let* packet, more_data = read_packet ~ctx ?more_data () in
          handle_packet ~ctx ~packet ~more_data
        | `Bandwidth bwm ->
          begin match bwm.Protocol.T.direction with
            | `Up ->
              let protocol = Some protocol in
              O.received_packet ~conn_id ~ip:dst ~port:dst_port
                ~header ~protocol;
              let* more_data =
                read_n_packets_ignoring_data
                  ~ctx
                  ~n:bwm.Protocol.T.n_packets
                  ~more_data
              in
              let* packet, more_data = read_packet ~ctx ?more_data () in
              handle_packet ~ctx ~packet ~more_data
            | `Down -> 
              let protocol = Some protocol in
              O.received_packet ~conn_id ~ip:dst ~port:dst_port
                ~header ~protocol;
              let n = bwm.Protocol.T.n_packets in
              let data =
                String.make bwm.packet_size '%'
                |> Cstruct.of_string
              in
              let* () = respond_with_n_copies ~ctx ~n ~header ~data in
              let* packet, more_data = read_packet ~ctx ?more_data () in
              handle_packet ~ctx ~packet ~more_data
          end
        | `Latency `Ping ->
          let protocol = Some protocol in
          O.received_packet ~conn_id ~ip:dst ~port:dst_port
            ~header ~protocol;
          let header = packet.header in
          let protocol = `Latency `Pong in
          let* () = respond ~ctx ~header ~protocol in
          let* packet, more_data = read_packet ~ctx ?more_data () in
          handle_packet ~ctx ~packet ~more_data
        | `Latency `Pong -> 
          let protocol = Some protocol in
          O.received_packet ~conn_id ~ip:dst ~port:dst_port
            ~header ~protocol;
          let* packet, more_data = read_packet ~ctx ?more_data () in
          handle_packet ~ctx ~packet ~more_data
      and respond ~ctx ~header ~protocol =
        let { flow; dst; dst_port; conn_id } = ctx in
        let data = protocol |> Protocol.to_cstruct in
        let response = Packet.to_cstructs ~header ~data in
        S.TCP.writev flow response >>= function
        | Ok () ->
          ctx.progress () >>= fun () ->
          let protocol = Some protocol in
          O.sent_packet ~conn_id ~ip:dst ~port:dst_port ~header ~protocol;
          Lwt_result.return ()
        | Error err ->
          let msg = Fmt.str "%a" S.TCP.pp_write_error err in
          Lwt_result.fail @@ `Msg msg
      and respond_with_n_copies ~ctx ~n ~header ~data =
        if n <= 0 then Lwt_result.return () else 
          let { flow; dst; dst_port; conn_id } = ctx in
          let response = Packet.to_cstructs ~header ~data in
          S.TCP.writev flow response >>= function
          | Ok () ->
            ctx.progress () >>= fun () ->
            let protocol = None in
            O.sent_packet ~conn_id ~ip:dst ~port:dst_port ~header ~protocol;
            let n = pred n in
            respond_with_n_copies ~ctx ~n ~header ~data 
          | Error err ->
            let msg = Fmt.str "%a" S.TCP.pp_write_error err in
            Lwt_result.fail @@ `Msg msg
      in
      let callback flow =
        Mirage_runtime.at_exit (fun () -> S.TCP.close flow);
        let dst, dst_port = S.TCP.dst flow in
        let conn_id = Uuidm.(v `V4 |> to_string) in
        O.new_connection ~conn_id ~ip:dst ~port:dst_port;
        Lwt.catch
          (fun () ->
              let timeout_ns = ns_of_sec (float timeout) in
              let timeout_state = Timeout.make ~timeout_ns in
              let progress () = Timeout.progress timeout_state in
              let ctx = { flow; dst; dst_port; conn_id; progress } in
              let handle_t =
                let* packet, more_data = read_packet ~ctx () in 
                handle_packet ~ctx ~packet ~more_data
              in
              Timeout.cancel_on_timeout timeout_state handle_t;
              handle_t >>= function
              | Ok () ->
                O.closing_connection ~conn_id ~ip:dst ~port:dst_port;
                S.TCP.close flow
              | Error `Msg err ->
                O.error ~conn_id ~ip:dst ~port:dst_port ~err;
                O.closing_connection ~conn_id ~ip:dst ~port:dst_port;
                S.TCP.close flow
          )
          (fun exn -> 
              let err = Printexc.to_string exn in
              O.error ~conn_id ~ip:dst ~port:dst_port ~err;
              O.closing_connection ~conn_id ~ip:dst ~port:dst_port;
              S.TCP.close flow
          )
      in
      Mirage_runtime.at_exit (fun () ->
        S.TCP.unlisten (S.tcp Sv.stack) ~port |> Lwt.return
      );
      S.TCP.listen (S.tcp Sv.stack) ~port callback;
      O.registered_listener ~port

    let udp ~name ~port ~timeout =
      let module O = O.Listen.Udp in
      let callback ~src:_ ~dst ~src_port:_ data =
        O.data ~ip:dst ~port ~data;
        Lwt.return_unit
      in
      Mirage_runtime.at_exit (fun () ->
        S.UDP.unlisten (S.udp Sv.stack) ~port |> Lwt.return
      );
      S.UDP.listen (S.udp Sv.stack) ~port callback;
      O.registered_listener ~port

  end

  module Connect = struct

    type context = {
      flow : S.TCP.flow;
      packet_index : int;
      conn_id : string;
      progress : unit -> unit Lwt.t;
    }
    
    let sleep_ns_before_retry = ns_of_sec 1.

    let tcp ~name ~port ~ip ~monitor_bandwidth ~timeout =
      let open Lwt_result.Syntax in
      let module O = O.Connect.Tcp in
      let bandwidth_testdata_str =
        String.make monitor_bandwidth#packet_size '%' in
      let bandwidth_testdata = Cstruct.of_string bandwidth_testdata_str in
      let n_bandwidth_packets =
        2000. *. 128e3 /. float monitor_bandwidth#packet_size |> truncate
      in
      let conn_id = Uuidm.(v `V4 |> to_string) 
      in
      let rec loop_try_connect () =
        O.connecting ~conn_id ~ip ~port;
        S.TCP.create_connection (S.tcp Sv.stack) (ip, port)
        >>= function
        | Error err ->
          let err = Fmt.str "%a" S.TCP.pp_error err in
          O.error_connection ~conn_id ~ip ~port ~err;
          Time.sleep_ns sleep_ns_before_retry >>= fun () ->
          loop_try_connect ()
        | Ok flow ->
          O.connected ~conn_id ~ip ~port;
          Mirage_runtime.at_exit (fun () -> S.TCP.close flow);
          let timeout_ns = ns_of_sec (float timeout) in
          let timeout_state = Timeout.make ~timeout_ns in
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
            O.closing_flow ~conn_id ~ip ~port;
            S.TCP.close flow >>= fun () ->
            O.closed_flow ~conn_id ~ip ~port;
            Time.sleep_ns sleep_ns_before_retry >>= fun () ->
            loop_try_connect ()
          | Error err ->
            O.error ~conn_id ~ip ~port ~err;
            O.closing_flow ~conn_id ~ip ~port;
            S.TCP.close flow >>= fun () ->
            O.closed_flow ~conn_id ~ip ~port;
            Time.sleep_ns sleep_ns_before_retry >>= fun () ->
            loop_try_connect ()
      and write_more ~ctx ~conn_state =
        let header = Packet.T.{
          index = ctx.packet_index;
          connection_id = ctx.conn_id
        } in
        match conn_state with
        | `Init ->
          let protocol = `Hello Protocol.T.{ name } in
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
            connection_id = ctx.conn_id
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
          let protocol = `Bandwidth Protocol.T.{
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
          let protocol = `Bandwidth Protocol.T.{
            direction;
            n_packets = n_bandwidth_packets; 
            packet_size = monitor_bandwidth#packet_size;
          } in 
          let* ctx = write_packet ~ctx ~header ~protocol in
          let* () = read_n_packets_ignoring_data ~ctx ~n:n_bandwidth_packets in
          let conn_state = `Latency in
          write_more ~ctx ~conn_state
      and write_packet ~ctx ~header ~protocol =
        let data = Protocol.to_cstruct protocol in
        Packet.to_cstructs ~header ~data
        |> S.TCP.writev ctx.flow >>= function
        | Ok () ->
          ctx.progress () >>= fun () ->
          let protocol = Some protocol in
          O.sent_packet ~conn_id ~ip ~port ~header ~protocol;
          let ctx =
            let packet_index = succ ctx.packet_index in
            { ctx with packet_index }
          in
          Lwt_result.return ctx
        | Error private_err ->
          let err = match private_err with
            | (#Tcpip.Tcp.write_error as err) -> Some err
            | _ -> None
          in
          let msg = Fmt.str "%a" S.TCP.pp_write_error private_err in
          Lwt_result.fail @@ `Msg msg
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
            }
            in
            let data = Packet.to_cstructs ~header ~data in
            S.TCP.writev ctx.flow data >>= function
            | Ok () ->
              ctx.progress () >>= fun () ->
              let protocol = None in
              O.sent_packet ~conn_id ~ip ~port ~header ~protocol;
              loop ~packet_index:(succ packet_index) (pred n)
            | Error private_err ->
              let err = match private_err with
                | (#Tcpip.Tcp.write_error as err) -> Some err
                | _ -> None
              in
              let msg = Fmt.str "%a" S.TCP.pp_write_error private_err in
              Lwt_result.fail @@ `Msg msg
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
            S.TCP.read ctx.flow
        in
        input_t
        >>= function
        | Ok `Eof -> Lwt_result.fail `Eof
        | Ok (`Data data) ->
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
                    |> Protocol.of_cstruct
                    |> Lwt.return
                  in
                  Some protocol
              in
              (*> gomaybe fail on wrong packet recived? - optimistic makes sense,
                  .. as there can be so many edgecases that I don't want to catch
                     * < this would need some 'expected' param, or returning header * protocol
              *)
              O.received_packet ~conn_id ~ip ~port ~header ~protocol;
              more_data
            | `Unfinished packet ->
              read_packet ~ctx ~unfinished_packet:packet ~ignore_protocol ()
          end
        | Error private_err -> 
          let msg = Fmt.str "%a" S.TCP.pp_error private_err in
          let err = match private_err with
            | (#Tcpip.Tcp.error as err) -> Some err
            | _ -> None
          in
          (* Lwt_result.fail @@ `Read (err, msg) *)
          Lwt_result.fail @@ `Msg msg
      in
      loop_try_connect ()

    (*goto
      * this should loop sending packets
        * and retry + print on some failure
          * ! have in mind that with notty one can't print
            * - so this should later be replaced by updating react graph with error
      * this should send the index of the packet
        * this way one can count what packets has been lost @ listener
      * the listener should send a packet back with same index (right away)
        * so this conntest can check what (roundtrip) latency was of that packet-index
    *)
    let udp ~name ~port ~ip ~monitor_bandwidth ~timeout =
      let module O = O.Connect.Udp in
      let data_str = "I'm "^name in
      let data = Cstruct.of_string data_str in
      O.writing ~ip ~port ~data:data_str;
      S.UDP.write ~dst:ip ~dst_port:port (S.udp Sv.stack) data >>= function
      | Ok () -> Lwt.return_unit
      | Error udp_err -> 
        let _err = Fmt.str "%a" S.UDP.pp_error udp_err in
        (*> goto add*)
        (* O.error *)
        Lwt.return_unit

  end

end
