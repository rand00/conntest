open Lwt.Infix

let (let*) = Result.bind 
let (let+) x f = Result.map f x 

let result_of_opt msg = function
  | Some v -> Ok v
  | None -> Error (`Msg msg)

let lwt_result_flatten_result = function
  | Ok result_t -> result_t
  | Error _ as err -> Lwt.return err

module Main (S : Tcpip.Stack.V4V6) = struct

  module Listen = struct
    
    let tcp stack port =
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

  let try_register_listener ~stack input =
    begin match input with
      | "tcp" :: port :: [] ->
        begin match int_of_string_opt port with
          | Some port -> Listen.tcp stack port |> Result.ok
          | None ->
            let msg =
              Fmt.str "Error: try_register_listener: Port '%s' is malformed" port
            in
            Error (`Msg msg)
        end
      | "udp" :: port :: [] ->
        begin match int_of_string_opt port with
          | Some port -> Listen.udp stack port |> Result.ok
          | None ->
            let msg =
              Fmt.str "Error: try_register_listener: Port '%s' is malformed" port
            in
            Error (`Msg msg)
        end
      | protocol :: _port :: [] -> 
        let msg = 
          Fmt.str "Error: try_register_listener: Protocol '%s' not supported"
            protocol
        in
        Error (`Msg msg)
      | strs -> 
        let msg = 
          Fmt.str "Error: try_register_listener: Bad format given to --listen. \
                   You passed: '%s'"
            (String.concat ":" strs)
        in
        Error (`Msg msg)
    end
    |> function
    | Ok () -> Lwt.return_unit
    | Error (`Msg msg) ->
      Logs.err (fun m -> m "Error: try_register_listener: %s" msg);
      exit 1

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
  
  let try_initiate_connection ~stack ~name uri_str =
    begin
      let uri = Uri.of_string uri_str in
      let* protocol =
        let* protocol_str =
          Uri.scheme uri
          |> result_of_opt (
            Fmt.str "Protocol was not defined in URI '%s'" uri_str
          )
        in
        match protocol_str with
        | "tcp" -> Ok `Tcp
        | "udp" -> Ok `Udp
        | _ -> Error (
          `Msg (Fmt.str "Protocol '%s' is not supported" protocol_str)
        )
      in
      let* ip =
        let* ip_str =
          Uri.host uri
          |> result_of_opt (
            Fmt.str "IP was not present in URI '%s'" uri_str
          )
        in
        Ipaddr.of_string ip_str
      in
      let* port =
        Uri.port uri
        |> result_of_opt (
          Fmt.str "Port was not defined in URI '%s'" uri_str
        )
      in
      let options = Uri.query uri in
      let+ monitor_bandwidth =
        options |> List.fold_left (fun acc option ->
          let* acc = acc in
          match option with 
          | "monitor-bandwidth", [] -> Ok true
          | option_name, _ ->
            let msg = Fmt.str "Unknown option: '%s'" option_name in
            Error (`Msg msg)
        ) (Ok false)
      in
      match protocol with
      | `Tcp -> Connect.tcp ~stack ~name ~port ~ip ~monitor_bandwidth
      | `Udp -> Connect.udp ~stack ~name ~port ~ip ~monitor_bandwidth
    end
    |> lwt_result_flatten_result >>= function
    | Ok () -> Lwt.return_unit
    | Error (`Msg msg) ->
      Logs.err (fun m -> m "Error: try_initiate_connection: %s" msg);
      exit 1
    | Error (`Udp udp_err) ->
      Logs.err (fun m -> m "Error: try_initiate_connection: %a"
          S.UDP.pp_error udp_err
      );
      exit 1
    | Error (`Tcp (`Connection tcp_err)) ->
      Logs.err (fun m -> m "Error: try_initiate_connection: %a"
          S.TCP.pp_error tcp_err
      );
      exit 1
    | Error (`Tcp (`Write tcp_err)) ->
      Logs.err (fun m -> m "Error: try_initiate_connection: %a"
          S.TCP.pp_write_error tcp_err
      );
      exit 1

  let start stack =
    let name = Key_gen.name () in
    Lwt.async begin fun () -> 
      Key_gen.listen ()
      |> Lwt_list.iter_p (try_register_listener ~stack)
    end;
    Lwt.async begin fun () -> 
      Key_gen.connect ()
      |> Lwt_list.iter_p (try_initiate_connection ~stack ~name)
    end;
    S.listen stack

end
