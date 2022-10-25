open Lwt.Infix
open Lwt_result.Syntax

module Output = Output

let (let*) = Result.bind 
let (let+) x f = Result.map f x 

module type S = sig
  
  module Listen : sig

    val tcp : name:string -> port:int -> timeout:int -> unit
    val udp : name:string -> port:int -> timeout:int -> unit

  end

  module Connect : sig

    val tcp :
      name:string ->
      port:int ->
      ip:Ipaddr.t ->
      monitor_bandwidth:< enabled : bool; packet_size : int; .. > ->
      timeout:int ->
      'a Lwt.t

    (*> goto add control of packet ringbuffer*)
    val udp :
      name:string ->
      port:int ->
      ip:Ipaddr.t ->
      monitor_bandwidth:< enabled : bool; packet_size : int; .. > ->
      timeout:int ->
      'a Lwt.t

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

    let create_connection dst =
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
    }

    let udp_stack = S.udp Sv.stack

    (*> goto pass flow to callback*)
    (*goto howto;
      * read src-ip, src-port, packet-header-conn_id
        * put these into some datastructure tracking all 'connections'
          * brian; mutable?
          * then some single recursive proc reads all input-datagrams
            * and puts them into corresponding flow's based on conn-id
    *)
    let listen ~port user_callback =
      let callback ~src ~dst ~src_port data =
        match Packet.Tcp.init ~ignore_data:true data with
        | Ok (`Done (packet, _rest)) ->
          (*goto check/register connection-id -> flow*)
          (*goto only call user-callback with flow on first packet recvd*)
          let source = Lwt_mvar.create @@ `Data data in
          let flow = { source; pier = src; pier_port = src_port } in
          user_callback flow
        (*goo*)
        (*> goto change interface of 'listen' to return Result.t instead*)
        | Ok (`Unfinished _) ->
          failwith "Udp_flow: `Unfinished is unsupported for UDP"
        | Error (`Msg err) ->
          failwith ("Udp_flow: Error: "^err) 
      in
      S.UDP.listen udp_stack ~port callback

    let unlisten ~port =
      S.UDP.unlisten udp_stack ~port 

    let create_connection (pier, pier_port) =
      let source = Lwt_mvar.create_empty () in
      Lwt_result.return { source; pier; pier_port }

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
    
    (*goto should more happen here?*)
    let close flow = Lwt.return_unit
    
  end
  
  module Udp = Protocol.Make(Time)(Udp_flow)(O)(struct
    let subproto = `Udp
  end)

  module Listen = struct

    let tcp = Tcp.Listen.start 

    let udp ~name ~port ~timeout =
      failwith "todo"

  end

  module Connect = struct
    
    let tcp = Tcp.Connect.start

    let udp ~name ~port ~ip ~monitor_bandwidth ~timeout =
      failwith "todo"

  end

end
