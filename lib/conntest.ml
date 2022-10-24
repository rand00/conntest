open Lwt.Infix
open Lwt_result.Syntax

module Output = Output

let (let*) = Result.bind 
let (let+) x f = Result.map f x 

module type S = sig

  type stack
  type udp_error
  
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
    (Tcp_output : Output.S)
    (Udp_output : Output.S)
= struct

  (* type stack = S.t *)

  module Tcp_flow = struct
    include S.TCP

    type t = S.TCP.flow

    let tcp_stack = S.tcp Sv.stack
    
    let error_to_msg pp v =
      v >>= function
      | Ok v -> Lwt_result.return v
      | Error err ->
        let msg = Fmt.str "%a" pp err in
        Lwt_result.fail @@ `Msg msg

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
  
  module Tcp = Protocol.Make(Time)(Tcp_flow)(Tcp_output)

  module Listen = struct

    let tcp ~name ~port ~timeout =
      failwith "todo"

    let udp ~name ~port ~timeout =
      failwith "todo"

  end

  module Connect = struct
    
    let tcp ~name ~port ~ip ~monitor_bandwidth ~timeout =
      failwith "todo"

    let udp ~name ~port ~ip ~monitor_bandwidth ~timeout =
      failwith "todo"

  end

end
