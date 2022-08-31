open Lwt.Infix

let (let*) = Result.bind 
let (let+) x f = Result.map f x 

let result_of_opt msg = function
  | Some v -> Ok v
  | None -> Error (`Msg msg)

let lwt_result_flatten_result = function
  | Ok result_t -> result_t
  | Error _ as err -> Lwt.return err

module Main
    (C : Mirage_console.S)
    (Notty_term : Notty_mirage.TERM)
    (Time : Mirage_time.S)
    (S : Tcpip.Stack.V4V6)
= struct

  module Output = Conntest.Output.Log_stdout
  module Ct = Conntest.Make(S)(Output)
  
  let try_register_listener ~stack input =
    begin match input with
      | "tcp" :: port :: [] ->
        begin match int_of_string_opt port with
          | Some port -> Ct.Listen.tcp stack port |> Result.ok
          | None ->
            let msg =
              Fmt.str "try_register_listener: Port '%s' is malformed" port
            in
            Error (`Msg msg)
        end
      | "udp" :: port :: [] ->
        begin match int_of_string_opt port with
          | Some port -> Ct.Listen.udp stack port |> Result.ok
          | None ->
            let msg =
              Fmt.str "try_register_listener: Port '%s' is malformed" port
            in
            Error (`Msg msg)
        end
      | protocol :: _port :: [] -> 
        let msg = 
          Fmt.str "try_register_listener: Protocol '%s' not supported"
            protocol
        in
        Error (`Msg msg)
      | strs -> 
        let msg = 
          Fmt.str "try_register_listener: Bad format given to --listen. \
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
          let* _acc = acc in
          match option with 
          | "monitor-bandwidth", [] -> Ok true
          | option_name, _ ->
            let msg = Fmt.str "Unknown option: '%s'" option_name in
            Error (`Msg msg)
        ) (Ok false)
      in
      match protocol with
      | `Tcp -> Ct.Connect.tcp ~stack ~name ~port ~ip ~monitor_bandwidth
      | `Udp -> Ct.Connect.udp ~stack ~name ~port ~ip ~monitor_bandwidth
    end
    |> lwt_result_flatten_result >>= function
    | Ok () -> Lwt.return_unit
    | Error (`Msg msg) ->
      Logs.err (fun m -> m "Error: try_initiate_connection: %s" msg);
      exit 1
    (*> goto these should be handled inside Ct as it should loop and try connection*)
    | Error (`Udp udp_err) ->
      Logs.err (fun m ->
        m "Error: try_initiate_connection: %a"
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

  let test_image ~dims:_ i =
    let open Notty in
    I.string ~attr:A.(bg red) @@ Fmt.str "%d" i
  
  let test_notty ~console ~init_size () =
    let open Lwt_result.Syntax in
    begin
      let* term = Notty_term.create ~init_size console in
      Mirage_runtime.at_exit (fun () ->
        Notty_term.close term
      );
      let fps = 60. in
      let fps_sleep_ns = 1e9 /. fps in
      let rec loop_render i =
        Time.sleep_ns @@ Int64.of_float fps_sleep_ns >>= fun () ->
        let image = test_image ~dims:init_size i in
        let* () = Notty_term.write term @@ `Image image in
        loop_render @@ succ i
      in
      loop_render 0
    end
    |> Lwt.map (function
      | Ok () -> ()
      | Error _ ->
        Logs.err (fun m -> m "ERROR: test_notty")
    )

  let start _console _notty _time stack =
    let _term_size = 70, 11 in
    (* Lwt.async @@ test_notty ~console ~init_size:term_size; *)
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
