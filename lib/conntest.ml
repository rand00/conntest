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
        | Ok (`Data data) ->
          read_and_respond ~flow ~dst ~dst_port ~data ~unfinished_packet
      and read_and_respond ~flow ~dst ~dst_port ~data ~unfinished_packet =
        let* unfinished = match unfinished_packet with
          | None -> Packet.Tcp.init data |> Lwt.return
          | Some unfinished ->
            Packet.Tcp.append ~data unfinished |> Lwt.return
        in
        begin match unfinished with
          | `Unfinished packet ->
            loop_read ~flow ~dst ~dst_port @@ Some packet
          | `Done (packet, more_data) ->
            O.packet ~ip:dst ~port:dst_port packet;
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
                begin match more_data with
                  | None -> loop_read ~flow ~dst ~dst_port None
                  | Some data ->
                    read_and_respond ~flow ~dst ~dst_port ~data
                      ~unfinished_packet
                end
              | Error err ->
                let msg = Fmt.str "%a" S.TCP.pp_write_error err in
                Lwt_result.fail @@ `Msg msg
            end
        end
      in
      let callback flow =
        Mirage_runtime.at_exit (fun () -> S.TCP.close flow);
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

    let sleep_ns_before_retry = sec 1.

    let bandwidth_testdata_str = String.make 5_000_000 '%'
    let bandwidth_testdata = Cstruct.of_string bandwidth_testdata_str
    
    (*Note: howto: loop sending packets, to:
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
          Time.sleep_ns sleep_ns_before_retry >>= fun () ->
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
        | Error private_err -> 
          let msg = Fmt.str "%a" S.TCP.pp_error private_err in
          let err = match private_err with
            | (#Tcpip.Tcp.error as err) -> Some err
            | _ -> None
          in
          Lwt_result.fail @@ `Read (err, msg)
      and loop_write ~index ~connection_id flow =
        (*> goto for bandwidth monitoring, create packets of CLI specified size*)
        let sleep_secs = if monitor_bandwidth then 0.0 else 0.2 in 
        let header = Packet.T.{ index; connection_id } in
        let data = if monitor_bandwidth then bandwidth_testdata else
            Cstruct.of_string name
        in
        O.writing ~ip ~port ~data;
        Packet.to_cstructs ~header ~data
        |> S.TCP.writev flow >>= function
        | Ok () ->
          O.wrote_data ~ip ~port;
          begin
            (*> goto goo - avoid reading based on monitor_bandwidth
              .. also need to tell server (via data or header?) that it shouldn't
              .. send a response (send this info in each packet)
                * and then once in a while ask for a response -
                .. so latency can get updated while doing bandwidth-monitoring 
            *)
            loop_read_returning flow >>= function
            | Ok _response -> (*goto use response for stats*)
              Time.sleep_ns @@ sec sleep_secs >>= fun () ->
              loop_write ~index:(succ index) ~connection_id flow
            | Error err ->
              O.error_reading ~ip ~port ~err;
              O.closing_flow ~ip ~port;
              S.TCP.close flow >>= fun () ->
              O.closed_flow ~ip ~port;
              Time.sleep_ns sleep_ns_before_retry >>= fun () ->
              loop_try_connect ()
          end
        | Error private_err ->
          let err = match private_err with
            | (#Tcpip.Tcp.write_error as err) -> Some err
            | _ -> None
          in
          let msg = Fmt.str "%a" S.TCP.pp_write_error private_err in
          O.error_writing ~ip ~port ~err ~msg;
          O.closing_flow ~ip ~port;
          S.TCP.close flow >>= fun () ->
          O.closed_flow ~ip ~port;
          Time.sleep_ns sleep_ns_before_retry >>= fun () ->
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
