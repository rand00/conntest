open Lwt.Infix

module Output = Output

let (let*) = Result.bind 
let (let+) x f = Result.map f x 

let ns_of_sec n = Int64.of_float (1e9 *. n)


module type S = sig

  type stack
  type udp_error
  
  module Listen : sig

    val tcp : name:string -> port:int -> unit
    val udp : name:string -> port:int -> unit

  end

  module Connect : sig

    val tcp :
      name:string ->
      port:int ->
      ip:Ipaddr.t ->
      monitor_bandwidth:< enabled : bool; packet_size : int; .. > ->
      'a Lwt.t

    val udp :
      name:string ->
      port:int ->
      ip:Ipaddr.t ->
      monitor_bandwidth:'a ->
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

  module Listen = struct

    type context = {
      flow : S.TCP.flow;
      dst : Ipaddr.t;
      dst_port : int;
      conn_id : string;
      conn_state : Protocol.server_connection_state;
    }

    let tcp ~name ~port =
      let module O = O.Listen.Tcp in
      let open Lwt_result.Syntax
      in
      (*> goto pass 'more_data' or change name to not be used directly*)
      let rec read_more ~(ctx:context) ~unfinished_packet =
        let { flow; dst; dst_port; conn_id; conn_state } = ctx in
        Lwt.pick [
          (*> goto pass this timeout via cli?*)
          (Time.sleep_ns @@ ns_of_sec 10. >|= fun () -> `Timeout);
          (S.TCP.read flow >|= fun res -> `Progress res);
        ]
        >>= function
        | `Timeout -> Lwt_result.fail @@ `Msg "Timeout"
        | `Progress res -> 
          begin match res with 
            | Ok `Eof ->
              Logs.err (fun m -> m "DEBUG: RECEIVED EOF");
              Lwt_result.return ()
            | Ok (`Data data) ->
              O.received_data ~conn_id ~ip:dst ~port:dst_port data;
              (*> goto remove this option wrapper again as packet now supports full incremental read*)
              let data = Some data in
              loop_read_until_packet ~ctx ~data ~unfinished_packet
            | Error e ->
              let msg = Fmt.str "%a" S.TCP.pp_error e in
              Lwt_result.fail @@ `Msg msg
          end
      and loop_read_until_packet ~ctx ~data ~unfinished_packet =
          match data with
          | None -> read_more ~ctx ~unfinished_packet
          | Some data -> 
            let* unfinished =
              match unfinished_packet with
              | None ->
                (*goto remove debug*)
                Logs.err (fun m -> m "DEBUG: Packet.Tcp.init data");
                Packet.Tcp.init data |> Lwt.return
              | Some unfinished ->
                (*goto remove debug*)
                Logs.err (fun m -> m "DEBUG: Packet.Tcp.append ~data unfinished");
                Packet.Tcp.append ~data unfinished |> Lwt.return
            in
            match unfinished with
            | `Unfinished packet ->
              let unfinished_packet = Some packet in
              read_more ~ctx ~unfinished_packet
            | `Done (packet, more_data) ->
              (*goto remove debug*)
              let d_more_data =
                more_data
                |> Option.map Cstruct.to_string
                |> Option.value ~default:"None"
              in
              Logs.err (fun m -> m "DEBUG: more_data = %s" d_more_data);
              handle_packet ~ctx ~packet ~more_data
      and handle_packet ~ctx ~packet ~more_data =
        let { flow; dst; dst_port; conn_id; conn_state } = ctx in
        let header = packet.header in
        begin match conn_state with
          | `Bandwidth_packets_to_read n ->
            let protocol = None in
            O.received_packet ~conn_id ~ip:dst ~port:dst_port ~header ~protocol;
            let ctx =
              let conn_state =
                if n <= 0 then `Normal else 
                  `Bandwidth_packets_to_read (pred n) in
              { ctx with conn_state }
            in
            loop_read_until_packet ~ctx ~data:more_data ~unfinished_packet:None
          | `Normal -> 
            let* protocol = packet.data |> Protocol.of_string |> Lwt.return in
            begin match protocol with
              | `Hello hello ->
                let protocol = Some protocol in
                O.received_packet ~conn_id ~ip:dst ~port:dst_port
                  ~header ~protocol;
                let protocol = `Hello Protocol.T.{ name } in
                let* () = respond ~ctx ~header ~protocol in
                let data = more_data in
                loop_read_until_packet ~ctx ~data ~unfinished_packet:None
              | `Bandwidth bwm ->
                begin match bwm.Protocol.T.direction with
                  | `Up ->
                    let protocol = Some protocol in
                    O.received_packet ~conn_id ~ip:dst ~port:dst_port
                      ~header ~protocol;
                    let ctx = 
                      let conn_state =
                        `Bandwidth_packets_to_read bwm.Protocol.T.n_packets in
                      { ctx with conn_state }
                    in
                    let data = more_data in
                    loop_read_until_packet ~ctx ~data ~unfinished_packet:None
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
                    let data = more_data in
                    loop_read_until_packet ~ctx ~data ~unfinished_packet:None
                end
              | `Latency `Ping ->
                (*> goto write this proc more like Connect.tcp - seems cleaner*)
                let protocol = Some protocol in
                O.received_packet ~conn_id ~ip:dst ~port:dst_port
                  ~header ~protocol;
                let header = packet.header in
                let protocol = `Latency `Pong in
                let* () = respond ~ctx ~header ~protocol in
                let data = more_data in
                loop_read_until_packet ~ctx ~data ~unfinished_packet:None
              | `Latency `Pong -> 
                let protocol = Some protocol in
                O.received_packet ~conn_id ~ip:dst ~port:dst_port
                  ~header ~protocol;
                let data = more_data in
                loop_read_until_packet ~ctx ~data ~unfinished_packet:None
            end
        end
      and respond ~ctx ~header ~protocol =
        let { flow; dst; dst_port; conn_id; conn_state } = ctx in
        let data = protocol |> Protocol.to_cstruct in
        let response = Packet.to_cstructs ~header ~data in
        S.TCP.writev flow response >>= function
        | Ok () ->
          let protocol = Some protocol in
          O.sent_packet ~conn_id ~ip:dst ~port:dst_port ~header ~protocol;
          Lwt_result.return ()
        | Error err ->
          let msg = Fmt.str "%a" S.TCP.pp_write_error err in
          Lwt_result.fail @@ `Msg msg
      and respond_with_n_copies ~ctx ~n ~header ~data =
        if n <= 0 then Lwt_result.return () else 
          let { flow; dst; dst_port; conn_id; conn_state } = ctx in
          let response = Packet.to_cstructs ~header ~data in
          S.TCP.writev flow response >>= function
          | Ok () ->
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
        let conn_state = `Normal in
        let ctx = { flow; dst; dst_port; conn_id; conn_state } in
        O.new_connection ~conn_id ~ip:dst ~port:dst_port;
        Lwt.catch
          (fun () ->
              read_more ~ctx ~unfinished_packet:None >>= function
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

    let udp ~name ~port =
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
    }
    
    let sleep_ns_before_retry = ns_of_sec 1.

    (*> goto goo - still missing loop-retry etc*)
    let tcp ~name ~port ~ip ~monitor_bandwidth =
      let open Lwt_result.Syntax in
      let module O = O.Connect.Tcp in
      let bandwidth_testdata_str = String.make monitor_bandwidth#packet_size '%' in
      let bandwidth_testdata = Cstruct.of_string bandwidth_testdata_str in
      let n_bandwidth_packets = 20 in
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
          let ctx = {
            flow;
            conn_id;
            packet_index = 0;
          } in
          (*> goto should loop connect when this errors too
              .. ! and should handle closing of prev flow too on error*)
          write_more ~ctx ~conn_state:`Init >>= function
          | Ok _ ->
            (*goto signal this case to Output - but as what?*)
            O.closing_flow ~conn_id ~ip ~port;
            S.TCP.close flow >>= fun () ->
            O.closed_flow ~conn_id ~ip ~port;
            Time.sleep_ns sleep_ns_before_retry >>= fun () ->
            loop_try_connect ()
          | Error err ->
            O.error_reading ~conn_id ~ip ~port ~err;
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
          let conn_state =
            if monitor_bandwidth#enabled then 
              `Bandwidth `Up
            else
              `Latency
          in
          write_more ~ctx ~conn_state
        | `Bandwidth (`Up as direction) ->
          let n_packets = n_bandwidth_packets in
          let protocol = `Bandwidth Protocol.T.{
            direction;
            n_packets; (*goto control via CLI*)
            packet_size = monitor_bandwidth#packet_size;
          } in 
          let* ctx = write_packet ~ctx ~header ~protocol in
          let* ctx = write_n_copies ~ctx ~n:n_packets ~data:bandwidth_testdata in
          let conn_state = `Bandwidth `Down in
          write_more ~ctx ~conn_state
        | `Bandwidth (`Down as direction) ->
          let protocol = `Bandwidth Protocol.T.{
            direction;
            n_packets = n_bandwidth_packets; (*goto control via CLI*)
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
            (*> goto goo - check what this data is - if the error is here*)
            S.TCP.writev ctx.flow data >>= function
            | Ok () ->
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
            Lwt.return @@ `Progress (Ok (`Data more_data))
          | None ->
            Lwt.pick [
              (*> goto pass this timeout via cli*)
              (Time.sleep_ns @@ ns_of_sec 10. >|= fun () -> `Timeout);
              (S.TCP.read ctx.flow >|= fun res -> `Progress res);
            ]
        in
        input_t
        >>= function
        | `Timeout ->
          let err = "Conntest.Connect.Tcp.read_packet: Timeout" in
          Lwt_result.fail @@ `Msg err
        (*< goto should this return explicit error instead?
           .. at least should signal `Timeout to Output *)
        | `Progress res -> 
          begin match res with 
            | Ok `Eof -> Lwt_result.fail `Eof
            (*< goto check how this is handled outside - should actually be error
                when expecting packet!*)
            | Ok (`Data data) ->
              let* unfinished = match unfinished_packet with
                | None -> Packet.Tcp.init data |> Lwt.return
                | Some unfinished ->
                  Packet.Tcp.append ~data unfinished |> Lwt.return
              in
              begin match unfinished with
                | `Done (packet, more_data) ->
                  let header = packet.Packet.T.header in
                  let+ protocol =
                    if ignore_protocol then Lwt_result.return None else
                      let+ protocol =
                        packet.Packet.T.data
                        |> Protocol.of_string
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
                  read_packet ~ctx ~unfinished_packet:packet ()
              end
            | Error private_err -> 
              let msg = Fmt.str "%a" S.TCP.pp_error private_err in
              let err = match private_err with
                | (#Tcpip.Tcp.error as err) -> Some err
                | _ -> None
              in
              (* Lwt_result.fail @@ `Read (err, msg) *)
              Lwt_result.fail @@ `Msg msg
          end
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
    let udp ~name ~port ~ip ~monitor_bandwidth =
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
