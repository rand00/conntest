open Lwt.Infix

module Output = Output

let (let*) = Result.bind 
let (let+) x f = Result.map f x 


module Make (Time : Mirage_time.S) (S : Tcpip.Stack.V4V6) (O : Output.S) = struct

  module Listen = struct

    let tcp stack port =
      let module O = O.Listen.Tcp in
      let open Lwt_result.Syntax in
      let rec loop_read ~flow ~dst ~dst_port unfinished_packet =
        S.TCP.read flow >>= function
        | Ok `Eof ->
          O.closing_connection ~ip:dst ~port:dst_port;
          S.TCP.close flow >|= fun () -> Ok ()
        | Error e ->
          let msg = Fmt.str "%a" S.TCP.pp_error e in
          Lwt_result.fail (`Msg msg)
        (*< gomaybe try loop on timeout? - else can just wait on new req*)
        | Ok (`Data data) ->
          let* unfinished = match unfinished_packet with
            | None -> Packet.Tcp.init data |> Lwt.return
            | Some unfinished ->
              Packet.Tcp.append ~data unfinished |> Lwt.return
          in
          begin match unfinished with
            | `Done packet ->
              (*< goto there can be more data left that should be read as a new packet
                .. see https://serverfault.com/questions/534063/can-tcp-and-udp-packets-be-split-into-pieces
              *)
              O.packet ~ip:dst ~port:dst_port packet;
              (*goto goo send packet back (with empty data)*)
              let response_packet =
                Packet.T.{ packet with data = "" }
                |> Packet.to_string
                |> Cstruct.of_string
              in
              begin
                S.TCP.write flow response_packet >>= function
                | Ok () ->
                  (*> goto add this (naming?)*)
                  (* O.responded_to_packet ~ip ~port; *)
                  loop_read ~flow ~dst ~dst_port None
                | Error err ->
                  let msg = Fmt.str "%a" S.TCP.pp_write_error err in
                  Lwt_result.fail @@ `Msg msg
              end
            | `Unfinished packet ->
              loop_read ~flow ~dst ~dst_port @@ Some packet
          end
      in
      let callback flow =
        let dst, dst_port = S.TCP.dst flow in
        O.new_connection ~ip:dst ~port:dst_port;
        loop_read ~flow ~dst ~dst_port None >>= function
        | Ok () -> Lwt.return_unit
        | Error (`Msg err) ->
          O.error ~ip:dst ~port:dst_port ~err;
          O.closing_connection ~ip:dst ~port:dst_port;
          S.TCP.close flow 
      in
      Mirage_runtime.at_exit (fun () ->
        S.TCP.unlisten (S.tcp stack) ~port |> Lwt.return
      );
      S.TCP.listen (S.tcp stack) ~port callback;
      O.registered_listener ~port

    let udp stack port =
      let module O = O.Listen.Udp in
      let callback ~src:_ ~dst ~src_port:_ data =
        O.data ~ip:dst ~port ~data;
        Lwt.return_unit
      in
      Mirage_runtime.at_exit (fun () ->
        S.UDP.unlisten (S.udp stack) ~port |> Lwt.return
      );
      S.UDP.listen (S.udp stack) ~port callback;
      O.registered_listener ~port

  end

  module Connect = struct

    let sec n = Int64.of_float (1e9 *. n)
    
    (*goto loop sending packets, to:
      * check if connection is up
      * check latency
      * optionally monitor bandwidth
    *)
    let tcp ~stack ~name ~port ~ip ~monitor_bandwidth =
      let module O = O.Connect.Tcp in
      let rec loop_try_connect () =
        let connection_id = Uuidm.(v `V4 |> to_string) in
        O.connecting ~ip ~port;
        S.TCP.create_connection (S.tcp stack) (ip, port)
        >>= function
        | Error err ->
          let err = Fmt.str "%a" S.TCP.pp_error err in
          O.error_connection ~ip ~port ~err;
          Time.sleep_ns @@ sec 1. >>= fun () ->
          loop_try_connect ()
        | Ok flow ->
          O.connected ~ip ~port;
          Mirage_runtime.at_exit (fun () -> S.TCP.close flow);
          loop_write ~index:0 ~connection_id flow
      and loop_read_returning ?unfinished_packet flow =
        (*> goto add this*)
        (* O.reading_response ~ip ~port; *)
        S.TCP.read flow >>= function
        | Ok (`Data data) ->
          let open Lwt_result.Syntax in
          let* unfinished = match unfinished_packet with
            | None -> Packet.Tcp.init data |> Lwt.return
            | Some unfinished ->
              Packet.Tcp.append ~data unfinished |> Lwt.return
          in
          begin match unfinished with
            | `Done packet ->
              (*> goto add this ? - maybe call in loop_write on response?*)
              (* O.packet ~ip:dst ~port:dst_port packet; *)
              Lwt_result.return packet
            | `Unfinished packet ->
              loop_read_returning ~unfinished_packet:packet flow
          end
        | Ok `Eof -> Lwt_result.fail `Eof
        | Error read_err -> Lwt_result.fail @@ `Read read_err
      and loop_write ~index ~connection_id flow =
        (*> goto for bandwidth monitoring, create packets of CLI specified size*)
        let data = "I'm "^name in
        let packet_str =
          let open Packet.T in
          let header = { index; connection_id } in
          let packet = { header; data } in
          Packet.to_string packet
        in
        let packet = Cstruct.of_string packet_str in
        O.writing ~ip ~port ~data;
        S.TCP.write flow packet >>= function
        | Ok () ->
          O.wrote_data ~ip ~port;
          begin (*goo we are stuck here - does server send packet back?*)
            loop_read_returning flow >>= function
            | Ok _response -> (*goto use response for stats*)
              (*> goto shouldn't wait when bandwidth-monitoring*)
              Time.sleep_ns @@ sec 0.2 >>= fun () ->
              loop_write ~index:(succ index) ~connection_id flow
            | Error `Eof ->
              (* goto let O.error handle any kind of error instead ..*)
              (*> goto add this (name differently?)*)
              (* O.error_eof ~ip ~port; *)
              (*> goto reuse this 'failure case' boilerplate*)
              O.closing_flow ~ip ~port;
              S.TCP.close flow >>= fun () ->
              O.closed_flow ~ip ~port;
              (*> goto reuse wait-time-on-error definition*)
              Time.sleep_ns @@ sec 1. >>= fun () ->
              loop_try_connect ()
            | Error _ ->
              (*> goto add this *)
              (* O.error_reading ~ip ~port ~err; *)
              O.closing_flow ~ip ~port;
              S.TCP.close flow >>= fun () ->
              O.closed_flow ~ip ~port;
              (*> goto reuse wait-time-on-error definition*)
              Time.sleep_ns @@ sec 1. >>= fun () ->
              loop_try_connect ()
          end
        | Error (#Tcpip.Tcp.write_error as err) ->
          O.error_writing ~ip ~port ~err;
          O.closing_flow ~ip ~port;
          S.TCP.close flow >>= fun () ->
          O.closed_flow ~ip ~port;
          Time.sleep_ns @@ sec 1. >>= fun () ->
          loop_try_connect ()
        | Error private_err -> 
          let err = Fmt.str "%a" S.TCP.pp_write_error private_err in
          O.error_writing_str ~ip ~port ~err;
          O.closing_flow ~ip ~port;
          S.TCP.close flow >>= fun () ->
          O.closed_flow ~ip ~port;
          Time.sleep_ns @@ sec 1. >>= fun () ->
          loop_try_connect ()
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
    let udp ~stack ~name ~port ~ip ~monitor_bandwidth =
      let module O = O.Connect.Udp in
      let data_str = "I'm "^name in
      let data = Cstruct.of_string data_str in
      O.writing ~ip ~port ~data:data_str;
      S.UDP.write ~dst:ip ~dst_port:port (S.udp stack) data
      |> Lwt_result.map_error (fun err -> `Udp err)

  end

end
