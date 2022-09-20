
module T = struct

  type info = {
    name : string;
  }
  [@@deriving yojson]

  type direction = [ `Up | `Down ]
  [@@deriving yojson]

  type bandwidth_monitor = {
    direction : direction;
    n_packets : int;
    packet_size : int; (*bytes*)
  }
  [@@deriving yojson]

  type t = [
    | `Hello of info
    | `Bandwidth_monitor of bandwidth_monitor
    | `Latency of [ `Ping | `Pong ]
  ]
  [@@deriving yojson]

end
include T

let of_string str =
  str |> Yojson.Safe.from_string |> of_yojson
  |> Result.map_error (fun e -> `Msg e)
let to_string h = h |> to_yojson |> Yojson.Safe.to_string

let of_cstruct c = c |> Cstruct.to_string |> of_string
let to_cstruct v = v |> to_string |> Cstruct.of_string

(* let pseudocode_example_usage =
 *   let this = `Server in
 *   let module A = Protocol.Action in
 *   match A.of_cstruct packet.data with
 *   | `Hello info ->
 *     let hello_back = `Hello A.T.{ name = "foo" } |> A.to_cstruct in
 *     write hello_back >>= fun () ->
 *     default_loop_read ()
 *   | `Bandwidth_monitor { direction = `Up; n_packets; _ } ->
 *     loop_read_no_response ~n_packets >>= fun () ->
 *     default_loop_read ()
 *   | `Bandwidth_monitor { direction = `Down; n_packets; packet_size } ->
 *     let packet = make_packet packet_size in
 *     loop_write_no_response ~n_packets ~packet >>= fun () ->
 *     default_loop_read ()
 *   | `Latency `Ping ->
 *     let id = Uuidm.(v `V4 |> to_string) in
 *     O.ping ~id;
 *     let pong = `Latency `Pong |> A.to_cstruct in
 *     write flow pong >>= fun () ->
 *     read flow >>= fun data ->
 *     match A.of_cstruct data with
 *     | `Latency `Pong ->
 *       O.pong ~id;
 *       (\*< might be best like this, as there then is no need for
 *         identity of ping/pong*\)
 *     | _ ->
 *       O.err ..;
 *       default_loop_read () *)
    

