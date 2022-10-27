open Lwt.Infix
open Lwt_result.Syntax

module Output = Output

let (let*) = Result.bind 
let (let+) x f = Result.map f x 

module type Protocol_S = sig 

  module Listen : sig 
    val start : name:string -> port:int -> timeout:int -> unit
  end

  module Connect : sig
    val start :
      name:string ->
      port:int ->
      ip:Ipaddr.t ->
      monitor_bandwidth:< enabled : bool; packet_size : int; .. > ->
      timeout:int ->
      'a Lwt.t
  end

end


module type S = sig

  module Tcp : Protocol_S
  module Udp : Protocol_S

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

  let error_to_msg pp v =
    v >>= function
    | Ok v -> Lwt_result.return v
    | Error err ->
      let msg = Fmt.str "%a" pp err in
      Lwt_result.fail @@ `Msg msg

  module Tcp_flow = struct
    include S.TCP

    type t = S.TCP.flow

    let tcp_stack = S.tcp Sv.stack

    let listen ~port callback =
      S.TCP.listen tcp_stack ~port callback

    let unlisten ~port =
      S.TCP.unlisten tcp_stack ~port 

    let create_connection ~id:_ dst =
      S.TCP.create_connection tcp_stack dst
      |> error_to_msg S.TCP.pp_error

    let read flow = S.TCP.read flow |> error_to_msg S.TCP.pp_error

    let writev flow data =
      S.TCP.writev flow data |> error_to_msg S.TCP.pp_write_error

  end
  
  module Tcp = Protocol.Make(Time)(Tcp_flow)(O)(struct
    let subproto = `Tcp
  end)

  module Udp_flow = struct
    (* include S.UDP *)

    type t = {
      source : Cstruct.t Mirage_flow.or_eof Lwt_mvar.t;
      pier : Ipaddr.t;
      pier_port : int;
      conn_id : string;
    }

    let udp_stack = S.udp Sv.stack

    module Conn_map = Map.Make(String)

    (*> Warning: but don't know why you would run two instances of protocol*)
    let conn_map = ref (Conn_map.empty)

    let listen ~port user_callback =
      let callback ~src ~dst ~src_port data =
        match Packet.Tcp.init ~ignore_data:true data with
        | Ok (`Done (packet, _rest)) ->
          let conn_id = packet.Packet.T.header.connection_id in
          begin match Conn_map.find_opt conn_id !conn_map with
          | None -> 
            (*> goto only pass data here if packet-index = 0
              * in any case this should depend on ringbuffer (local to conn_id)
                * .. this can be put in 'flow'?
            *)
            let source = Lwt_mvar.create_empty () in  (* @@ `Data data *)
            (*> goto insert packet in ringbuffer*)
            let ringbuffer = failwith "todo" in
            (*goto startup async loop that inserts latest available
              ringbuffer packet in source-mvar
              * @problem; if user_callback doesn't loop quickly enough over
                source-mvar, then packets will get lost
                * @solution; make source-mvar into source-stream (infinite)
                  * this way:
                    * ringbuffer is only about receiving
                    * source-stream is only about buffering for user_callback
                      * @problem; can lead to memory-leak if user_callback is
                        generally too slow
                        * @solution; just use mvar - client shall be faster than
                          data comes in
            *)
            let flow = {
              source;
              pier = src;
              pier_port = src_port;
              conn_id;
              ringbuffer;
            } in
            let conn_map' = Conn_map.add conn_id flow !conn_map in
            conn_map := conn_map';
            (*> goto: this blocks, as the lifetime of this callback is longer
              * .. this depends on semantics of S.UDP.listen -
                * does it spin up all possible callbacks when recvd packet?
                  * or does it block recv nxt pkt on blocking callback?
              * @solution;
                * alternative is just to run this async
            *)
            user_callback flow
          | Some flow ->
            (*> goto do this in async ringbuffer loop instead*)
            (* Lwt_mvar.put flow.source @@ `Data data
            *)
            (*> goto insert packet in ringbuffer*)
            let ringbuffer = failwith "todo" in
            let flow = { flow with ringbuffer } in
            let conn_map' = Conn_map.add conn_id flow !conn_map in
            conn_map := conn_map';
            Lwt.return_unit
          end
        (*> goto change interface of 'listen' to return Result.t instead*)
        | Ok (`Unfinished _) ->
          failwith "Udp_flow: `Unfinished is unsupported for UDP"
        | Error (`Msg err) ->
          failwith ("Udp_flow: Error: "^err) 
      in
      S.UDP.listen udp_stack ~port callback

    let unlisten ~port =
      S.UDP.unlisten udp_stack ~port 

    let create_connection ~id (pier, pier_port) =
      let source = Lwt_mvar.create_empty () in
      Lwt_result.return { source; pier; pier_port; conn_id = id }

    let read flow =
      Lwt_mvar.take flow.source >|= fun res ->
      Ok res

    (*> Note: it's important that all cstructs are written at once for ordering*)
    let writev flow datas =
      let data = Cstruct.concat datas in
      let dst, dst_port = flow.pier, flow.pier_port in
      (*< spec:
        * listen-case: flow is only given to callback on recv first packet
        * connect-case: flow already has dst + dst_port
      *)
      S.UDP.write ~dst ~dst_port udp_stack data
      |> error_to_msg S.UDP.pp_error

    let dst flow = flow.pier, flow.pier_port
    
    let close flow =
      let conn_map' = Conn_map.remove flow.conn_id !conn_map in
      conn_map := conn_map';
      Lwt.return_unit
    
  end
  
  module Udp = Protocol.Make(Time)(Udp_flow)(O)(struct
    let subproto = `Udp
  end)

end
