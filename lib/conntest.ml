open Lwt.Infix

module Output = Output

let (let*) = Result.bind 
let (let+) x f = Result.map f x 

module Make (S : Tcpip.Stack.V4V6) (O : Output.S) = struct

  module Listen = struct

    let tcp stack port =
      let module O = O.Listen.Tcp in
      let callback flow =
        let dst, dst_port = S.TCP.dst flow in
        O.new_connection ~ip:dst ~port:dst_port;
        S.TCP.read flow >>= function
        | Ok `Eof ->
          O.closing_connection ~ip:dst ~port:dst_port;
          Lwt.return_unit
        | Error e ->
          let err = Fmt.str "%a" S.TCP.pp_error e in
          O.error ~ip:dst ~port:dst_port ~err;
          (*goto possibly add O.closing_connection *)
          Lwt.return_unit
        | Ok (`Data b) ->
          Logs.info (fun f ->
            f "read: %d bytes:\n%s" (Cstruct.length b) (Cstruct.to_string b));
          S.TCP.close flow
      in
      Mirage_runtime.at_exit (fun () ->
        S.TCP.unlisten (S.tcp stack) ~port |> Lwt.return
      );
      S.TCP.listen (S.tcp stack) ~port callback

    let udp stack port =
      let callback ~src:_ ~dst ~src_port:_ data =
        Logs.info (fun m ->
          m "new udp connection from IP '%s' on port '%d'"
            (Ipaddr.to_string dst) port);
        Logs.info (fun f ->
          f "read: %d bytes:\n%s" (Cstruct.length data) (Cstruct.to_string data));
        Lwt.return_unit
      in
      Mirage_runtime.at_exit (fun () ->
        S.UDP.unlisten (S.udp stack) ~port |> Lwt.return
      );
      S.UDP.listen (S.udp stack) ~port callback

  end

  module Connect = struct

    let tcp ~stack ~name ~port ~ip ~monitor_bandwidth =
      begin
        let open Lwt_result.Infix in
        S.TCP.create_connection (S.tcp stack) (ip, port)
        |> Lwt_result.map_error (fun err -> `Connection err)
        >>= fun flow ->
        Mirage_runtime.at_exit (fun () -> S.TCP.close flow);
        let data = Cstruct.of_string @@ "I'm "^name in
        S.TCP.write flow data
        |> Lwt_result.map_error (fun err -> `Write err)
      end
      |> Lwt_result.map_error (fun err -> `Tcp err)

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
      let data = Cstruct.of_string @@ "I'm "^name in
      S.UDP.write ~dst:ip ~dst_port:port (S.udp stack) data
      |> Lwt_result.map_error (fun err -> `Udp err)

  end

end
