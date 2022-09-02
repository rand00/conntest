open Lwt.Infix

module Output = Output

let (let*) = Result.bind 
let (let+) x f = Result.map f x 

(*goto make a module T here containing shared types for comms with serializers*)

module Make (Time : Mirage_time.S) (S : Tcpip.Stack.V4V6) (O : Output.S) = struct

  module Listen = struct

    (*goto loop read*)
    let tcp stack port =
      let module O = O.Listen.Tcp in
      let callback flow =
        let dst, dst_port = S.TCP.dst flow in
        O.new_connection ~ip:dst ~port:dst_port;
        S.TCP.read flow >>= function
        | Ok `Eof ->
          O.closing_connection ~ip:dst ~port:dst_port;
          S.TCP.close flow
        | Error e ->
          let err = Fmt.str "%a" S.TCP.pp_error e in
          O.error ~ip:dst ~port:dst_port ~err;
          (*goto possibly add O.closing_connection *)
          S.TCP.close flow
        (*< goto try loop on timeout? - else can just wait on new req*)
        | Ok (`Data data) ->
          (*goto depending on data -
            * latency-test: send packet back right away
            * bandwidth-test:
              * @brian howto;
                * just read as fast as possible
                  * and then send small pkt back when done?
                    * < latency can be subtracted
          *)
          O.data ~ip:dst ~port:dst_port ~data;
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
          loop_write flow
      and loop_write flow =
        let data_str = "I'm "^name in
        let data = Cstruct.of_string data_str in
        O.writing ~ip ~port ~data:data_str;
        S.TCP.write flow data >>= function
        | Ok () ->
          O.wrote_data ~ip ~port;
          (*goto receive response *)
          Time.sleep_ns @@ sec 0.2 >>= fun () ->
          loop_write flow
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
