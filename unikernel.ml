open Lwt.Infix

module Main (S : Tcpip.Stack.V4) = struct

  let listen_tcp stack port =
    let callback flow =
      let dst, dst_port = S.TCPV4.dst flow in
      Logs.info (fun m ->
        m "new tcp connection from IP %s on port %d"
          (Ipaddr.V4.to_string dst) dst_port);
      S.TCPV4.read flow >>= function
      | Ok `Eof ->
        Logs.info (fun f -> f "Closing connection!");
        Lwt.return_unit
      | Error e ->
        Logs.warn (fun f ->
          f "Error reading data from established connection: %a"
            S.TCPV4.pp_error e);
        Lwt.return_unit
      | Ok (`Data b) ->
        Logs.debug (fun f ->
          f "read: %d bytes:\n%s" (Cstruct.length b) (Cstruct.to_string b));
        S.TCPV4.close flow
    in
    S.TCPV4.listen (S.tcpv4 stack) ~port 

  let listen_udp stack port =
    let callback ~src ~dst ~src_port data =
      Logs.info (fun m ->
        m "new udp connection from IP %s on port %d"
          (Ipaddr.V4.to_string dst) port);
      Logs.debug (fun f ->
        f "read: %d bytes:\n%s" (Cstruct.length data) (Cstruct.to_string data));
      Lwt.return_unit
    in
    S.UDPV4.listen (S.udpv4 stack) ~port callback

  let start stack =
    (*    let port = Key_gen.port () in*)
    (*goto iter through list of --listen and register callbacks*)
    S.listen stack

end
