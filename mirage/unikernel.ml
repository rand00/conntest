open Lwt.Infix

let (let*) = Result.bind 
let (let+) x f = Result.map f x 

let result_of_opt msg = function
  | Some v -> Ok v
  | None -> Error (`Msg msg)

let lwt_result_flatten_result = function
  | Ok result_t -> result_t
  | Error _ as err -> Lwt.return err

let lwt_result_flip_result = function
  | Ok t -> t |> Lwt.map (fun v -> Ok v)
  | Error _ as err -> Lwt.return err

module Main
    (C : Mirage_console.S)
    (Notty_term : Notty_mirage.TERM)
    (Time : Mirage_time.S)
    (Clock : Mirage_clock.MCLOCK)
    (S : Tcpip.Stack.V4V6)
= struct

  (*Merlin-use line: notty.mirage notty lwt_react tcpip mirage-time mirage-console conntest*)
  
  let try_register_listener ~ct_m ~name input =
    let module Ct = (val ct_m : Conntest.S) in
    begin match input with
      | "tcp" :: port :: [] ->
        begin match int_of_string_opt port with
          | Some port -> Ct.Listen.tcp ~name ~port |> Result.ok
          | None ->
            let msg =
              Fmt.str "try_register_listener: Port '%s' is malformed" port
            in
            Error (`Msg msg)
        end
      | "udp" :: port :: [] ->
        begin match int_of_string_opt port with
          | Some port -> Ct.Listen.udp ~name ~port |> Result.ok
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

  let parse_file_size str =
    let open Astring in
    let (let*) x f = Option.bind x f
    and (let+) x f = Option.map f x
    in
    let* n, sub =
      let s = String.Sub.of_string str in
      match String.Sub.span ~min:1 ~sat:Char.Ascii.is_digit s with
      | (i, _) when String.Sub.is_empty i -> None
      | (i, sub) -> let+ i = String.Sub.to_int i in i, sub
    in
    let+ factor = match String.Sub.to_string sub |> String.Ascii.lowercase with
      | "b" | "" -> Some 1
      | "kb" -> Some 1_000
      | "mb" -> Some 1_000_000
      | "gb" -> Some 1_000_000_000
      | _ -> None
    in
    n * factor

  let find_option ~options tag ~default ~parse_vs =
    options |> List.fold_left (fun acc option ->
      let* (acc_v, acc_options) = acc in
      match option with 
      | tag', vs when String.equal tag tag' ->
        let+ v = parse_vs vs in
        v, acc_options
      | option -> Ok (acc_v, option :: acc_options)
    ) (Ok (default, []))

  let try_initiate_connection ~ct_m ~name uri_str =
    let module Ct = (val ct_m : Conntest.S) in
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
      let options = Uri.query uri
      in
      let* monitor_bandwidth_flag, options =
        let parse_vs = function
          | [] -> Ok true
          | _ -> Error (`Msg "'monitor-bandwidth' is a flag")
        in
        find_option ~options "monitor-bandwidth" ~default:false ~parse_vs
      in
      let* monitor_bandwidth_packet_size, options =
        let parse_vs = function
          | [ str ] ->
            parse_file_size str
            |> result_of_opt "bad packet-size given"
          | _ -> Error (`Msg "'packet-size' takes an argument like '2MB'")
        in
        find_option ~options "packet-size" ~default:5_000_000 ~parse_vs
      in
      let+ () = match options with
        | [] -> Ok ()
        | _  ->
          let options = List.map fst options in
          let msg =
            Fmt.str "Error: unknown options: [%s]" (String.concat ", " options)
          in
          Error (`Msg msg)
      in
      let monitor_bandwidth = object
        method enabled = monitor_bandwidth_flag
        method packet_size = monitor_bandwidth_packet_size
      end
      in
      match protocol with
      | `Tcp -> Ct.Connect.tcp ~name ~port ~ip ~monitor_bandwidth
      | `Udp -> Ct.Connect.udp ~name ~port ~ip ~monitor_bandwidth
    end
    |> lwt_result_flip_result >>= function
    | Ok () -> Lwt.return_unit
    | Error (`Msg msg) ->
      (*> goto pass to Ui instead*)
      Logs.err (fun m -> m "Error: try_initiate_connection: %s" msg);
      (*> goto it should be ensured that all errors exiting like this is
          caused by parsing of CLI args*)
      exit 64

  let render_ui ~init ~image_e ~dimensions_e ~console ~init_size () =
    let open Lwt.Infix in
    Notty_term.create ~init_size console >>= function
    | Error _ ->
      (*> goto can this put errors into notty-ui without divergence?*)
      Logs.err (fun m -> m "ERROR: render notty ui");
      Lwt.return_unit
    | Ok term ->
      begin
        Mirage_runtime.at_exit (fun () ->
          Notty_term.close term
        );
        image_e |> Lwt_react.E.map_s (fun image ->
          Notty_term.write term @@ `Image image >|= function
          | Ok () -> ()
          | Error _ -> Logs.err (fun m -> m "ERROR: render notty ui")
          (*< goto should always handle errors via Ui module*)
        ) |> Lwt_react.E.keep;
        dimensions_e |> Lwt_react.E.map (fun dims ->
          Notty_term.set_size term dims
        ) |> Lwt_react.E.keep;
        init ()
      end
  
  let start console _notty _time _clock stack =
    let term_dimensions = 50, 25 in
    let name = Key_gen.name () in
    let ui_key = match Key_gen.ui () with
      | "notty" -> `Notty
      | "log" -> `Log
      | ui_str ->
        Logs.err (fun m -> m "Unknown UI '%s'" ui_str);
        exit 64
    in
    let ui_m = match ui_key with
      | `Log -> (module Conntest.Output.Log_stdout () : Conntest.Output.S)
      | `Notty ->
        let module Ui = Conntest.Output.Notty_ui(Time)(Clock)(struct
          let name = name
          (* let term_dimensions = term_dimensions *)
        end)
        in
        Lwt.async @@ render_ui
          ~init:Ui.init
          ~image_e:Ui.image_e
          ~dimensions_e:Ui.Render.dimensions_e
          ~console
          ~init_size:term_dimensions;
        (module Ui.Input_event : Conntest.Output.S)
    in
    let module Ui = (val ui_m) in
    let module Stack_v = struct
      type t = S.t
      let stack = stack
    end in
    let module Ct = Conntest.Make(Time)(S)(Stack_v)(Ui) in
    let ct_m = (module Ct : Conntest.S)
    in
    Lwt.async begin fun () -> 
      Key_gen.listen ()
      |> Lwt_list.iter_p (try_register_listener ~ct_m ~name)
    end;
    Lwt.async begin fun () -> 
      Key_gen.connect ()
      |> Lwt_list.iter_p (try_initiate_connection ~ct_m ~name)
    end;
    S.listen stack

end
