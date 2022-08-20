open Lwt.Infix

module Main (S : Tcpip.Stack.V4V6) = struct

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
    Mirage_runtime.at_exit (fun () ->
      S.TCP.unlisten (S.tcp stack) ~port |> Lwt.return
    );
    S.TCP.listen (S.tcp stack) ~port callback

  let listen_udp stack port =
    let callback ~src:_ ~dst ~src_port:_ data =
      Logs.info (fun m ->
        m "new udp connection from IP '%s' on port '%d'"
          (Ipaddr.to_string dst) port);
      Logs.debug (fun f ->
        f "read: %d bytes:\n%s" (Cstruct.length data) (Cstruct.to_string data));
      Lwt.return_unit
    in
    Mirage_runtime.at_exit (fun () ->
      S.UDP.unlisten (S.udp stack) ~port |> Lwt.return
    );
    S.UDP.listen (S.udp stack) ~port callback

  let try_register_listener ~stack ~stop = function
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

  let log_none msg = function
    | None -> Logs.err (fun m -> m "%s" msg); None
    | Some _ as v -> v
  
  let try_initiate_connection ~stack ~stop uri_str =
    let uri = Uri.of_string uri_str in
    let (let*) = Option.bind in
    let (let+) x f = Option.map f x in
    let+ protocol =
      let* protocol_str =
        Uri.scheme uri
        (*> goto make this a Result instead, and log in the end?*)
        |> log_none (Fmt.str
            "Error: Protocol was not defined in uri '%s'" uri_str
        )
      in
      int_of_string_opt protocol_str
    in
    let ip =
      let* ip_str = Uri.host uri in
      Ipaddr.of_string ip_str |> Result.to_option
    in
    let options = Uri.query uri in
    failwith "todo"
  
  let start stack =
    let stop_t, stop =
      let mvar = Lwt_mvar.create_empty () in
      Lwt_mvar.take mvar, Lwt_mvar.put mvar
    in
    Lwt.async begin fun () -> 
      Key_gen.listen ()
      |> Lwt_list.iter_p (try_register_listener ~stack ~stop)
    end;
    Lwt.async begin fun () -> 
      Key_gen.connect ()
      |> Lwt_list.iter_p (try_initiate_connection ~stack ~stop)
    end;
    Lwt.pick [ stop_t; S.listen stack ]

end
