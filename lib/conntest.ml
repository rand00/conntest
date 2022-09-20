open Lwt.Infix

module Output = Output

let (let*) = Result.bind 
let (let+) x f = Result.map f x 

let ns_of_sec n = Int64.of_float (1e9 *. n)


module type S = sig

  type stack
  type udp_error
  
  module Listen : sig

    val tcp : int (*port*) -> unit
    val udp : int (*port*) -> unit

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

    let tcp port =
      let module O = O.Listen.Tcp in
      let open Lwt_result.Syntax in
      let rec loop_read ~flow ~dst ~dst_port ~conn_id ~unfinished_packet =
        Lwt.pick [
          (*> goto pass this timeout via cli?*)
          (Time.sleep_ns @@ ns_of_sec 10. >|= fun () -> `Timeout);
          (S.TCP.read flow >|= fun res -> `Progress res);
        ]
        >>= function
        | `Timeout ->
          O.closing_connection ~conn_id ~ip:dst ~port:dst_port;
          S.TCP.close flow >|= fun () -> Ok ()
        (*< goto should this timeout be signalled to Ui, so it can keep state for a while? *)
        | `Progress res -> 
          begin match res with 
            | Ok `Eof ->
              O.closing_connection ~conn_id ~ip:dst ~port:dst_port;
              S.TCP.close flow >|= fun () -> Ok ()
            | Error e ->
              let msg = Fmt.str "%a" S.TCP.pp_error e in
              Lwt_result.fail @@ `Msg msg
            | Ok (`Data data) ->
              read_and_respond
                ~flow
                ~dst
                ~dst_port
                ~data
                ~conn_id
                ~unfinished_packet
          end
      and read_and_respond
          ~flow
          ~dst
          ~dst_port
          ~data
          ~conn_id
          ~unfinished_packet
        =
        let* unfinished = match unfinished_packet with
          | None -> Packet.Tcp.init data |> Lwt.return
          | Some unfinished ->
            Packet.Tcp.append ~data unfinished |> Lwt.return
        in
        begin match unfinished with
          | `Unfinished packet ->
            let unfinished_packet = Some packet in
            loop_read
              ~flow
              ~dst
              ~dst_port
              ~conn_id
              ~unfinished_packet
          | `Done (packet, more_data) ->
            let header = packet.header in
            let* protocol =
              packet.data
              |> Protocol.of_string
              |> Result.map Option.some
              |> Lwt.return in
            O.received_packet ~conn_id ~ip:dst ~port:dst_port ~header ~protocol;
            let header = packet.header in
            let protocol = failwith "todo" in
            let response_packet_cstruct =
              { header; data = "" }
              |> Packet.to_string
              |> Cstruct.of_string
            in
            begin
              S.TCP.write flow response_packet_cstruct >>= function
              | Ok () ->
                O.sent_packet ~conn_id ~ip:dst ~port:dst_port ~header ~protocol;
                begin match more_data with
                  | None ->
                    let unfinished_packet = None in
                    loop_read ~flow ~dst ~dst_port ~conn_id ~unfinished_packet
                  | Some data ->
                    read_and_respond
                      ~flow
                      ~dst
                      ~dst_port
                      ~data
                      ~conn_id
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
        let conn_id = Uuidm.(v `V4 |> to_string) in
        O.new_connection ~conn_id ~ip:dst ~port:dst_port;
        Lwt.catch
          (fun () ->
             let unfinished_packet = None in
             loop_read ~flow ~dst ~dst_port ~conn_id ~unfinished_packet
             >>= function
             | Ok () -> Lwt.return_unit
             | Error `Msg err ->
               O.error ~conn_id ~ip:dst ~port:dst_port ~err;
               O.closing_connection ~conn_id ~ip:dst ~port:dst_port;
               S.TCP.close flow
          )
          (function (*goto - is this handled correctly?*)
            | exn ->
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

    let udp port =
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

    let sleep_ns_before_retry = ns_of_sec 1.

    (*Note: howto: loop sending packets, to:
      * check if connection is up
      * check latency
      * optionally monitor bandwidth
    *)
    let tcp ~name ~port ~ip ~monitor_bandwidth =
      let module O = O.Connect.Tcp in
      let bandwidth_testdata_str = String.make monitor_bandwidth#packet_size '%' in
      let bandwidth_testdata = Cstruct.of_string bandwidth_testdata_str
      in
      let rec loop_try_connect () =
        let conn_id = Uuidm.(v `V4 |> to_string) in
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
          loop_write ~index:0 ~conn_id flow
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
            | `Done packet_and_more_data ->
              Lwt_result.return packet_and_more_data
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
      and loop_write ~index ~conn_id flow =
        (*> goto for bandwidth monitoring, create packets of CLI specified size*)
        let sleep_secs = if monitor_bandwidth#enabled then 0.0 else 0.2 in 
        let header = Packet.T.{ index; connection_id = conn_id } in
        let data = if monitor_bandwidth#enabled then bandwidth_testdata else
            Cstruct.of_string name
        in
        O.writing ~conn_id ~ip ~port ~data;
        let protocol = failwith "todo" in
        Packet.to_cstructs ~header ~data
        |> S.TCP.writev flow >>= function
        | Ok () ->
          O.sent_packet ~conn_id ~ip ~port ~header ~protocol;
          begin
            loop_read_returning flow >>= function
            | Ok (response, _more_data) ->
              (*< goto use the 'more_data' if protocol specifies to read more*)
              let header = response.Packet.T.header in (*goo*)
              let protocol = failwith "todo" in
              O.received_packet ~conn_id ~ip ~port ~header ~protocol;
              (*> goto control if should sleep based on protocol instead*)
              Time.sleep_ns @@ ns_of_sec sleep_secs >>= fun () ->
              loop_write ~index:(succ index) ~conn_id flow
            | Error err ->
              O.error_reading ~conn_id ~ip ~port ~err;
              O.closing_flow ~conn_id ~ip ~port;
              S.TCP.close flow >>= fun () ->
              O.closed_flow ~conn_id ~ip ~port;
              Time.sleep_ns sleep_ns_before_retry >>= fun () ->
              loop_try_connect ()
          end
        | Error private_err ->
          let err = match private_err with
            | (#Tcpip.Tcp.write_error as err) -> Some err
            | _ -> None
          in
          let msg = Fmt.str "%a" S.TCP.pp_write_error private_err in
          O.error_writing ~conn_id ~ip ~port ~err ~msg;
          O.closing_flow ~conn_id ~ip ~port;
          S.TCP.close flow >>= fun () ->
          O.closed_flow ~conn_id ~ip ~port;
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
