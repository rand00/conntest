open Lwt.Infix

module Main (S : Tcpip.Stack.V4V6) = struct

  (*goto 'unlisten' at_exit*)
  (* Mirage_runtime.at_exit *)
  let listen_tcp stack port =
    let callback flow =
      let dst, dst_port = S.TCP.dst flow in
      Logs.info (fun m ->
        m "new tcp connection from IP '%s' on port '%d'"
          (Ipaddr.to_string dst) dst_port);
      S.TCP.read flow >>= function
      | Ok `Eof ->
        Logs.info (fun f -> f "Closing connection!");
        Lwt.return_unit
      | Error e ->
        Logs.warn (fun f ->
          f "Error reading data from established connection: %a"
            S.TCP.pp_error e);
        Lwt.return_unit
      | Ok (`Data b) ->
        Logs.debug (fun f ->
          f "read: %d bytes:\n%s" (Cstruct.length b) (Cstruct.to_string b));
        S.TCP.close flow
    in
    S.TCP.listen (S.tcp stack) ~port callback

  (*goto 'unlisten' at_exit*)
  let listen_udp stack port =
    let callback ~src:_ ~dst ~src_port:_ data =
      Logs.info (fun m ->
        m "new udp connection from IP '%s' on port '%d'"
          (Ipaddr.to_string dst) port);
      Logs.debug (fun f ->
        f "read: %d bytes:\n%s" (Cstruct.length data) (Cstruct.to_string data));
      Lwt.return_unit
    in
    S.UDP.listen (S.udp stack) ~port callback

  let start stack =
    let stop_t, stop =
      let mvar = Lwt_mvar.create_empty () in
      Lwt_mvar.take mvar, Lwt_mvar.put mvar
    in
    Lwt.async begin fun () -> 
      Key_gen.listen ()
      |> Lwt_list.iter_p (function
        | "tcp" :: port :: [] ->
          begin match int_of_string_opt port with
            | Some port -> listen_tcp stack port |> Lwt.return
            | None -> 
              Logs.err (fun m -> m "Error: Port '%s' is malformed" port);
              stop ()
          end
        | "udp" :: port :: [] ->
          begin match int_of_string_opt port with
            | Some port -> listen_udp stack port |> Lwt.return
            | None -> 
              Logs.err (fun m -> m "Error: Port '%s' is malformed" port);
              stop ()
          end
        | protocol :: _port :: [] -> 
          Logs.err (fun m -> m "Error: Protocol '%s' not supported" protocol);
          stop ()
        | strs -> 
          Logs.err (fun m ->
            m "Error: Bad format given to --listen. You passed: '%s'"
              (String.concat ":" strs)
          );
          stop ()
      )
    end;
    Lwt.pick [ stop_t; S.listen stack ]

end
