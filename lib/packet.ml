
(*> goto these are use in Unikernel too - could share*)
let (let*) = Result.bind 
let (let+) x f = Result.map f x 

let result_of_opt msg = function
  | Some v -> Ok v
  | None -> Error (`Msg msg)

module Raw = struct

  let parse_length data =
    let data_str = Cstruct.to_string data in
    let* newline_idx = 
      String.index_opt data_str '\n'
      |> result_of_opt "Packet.Raw.parse_length: No newline found"
    in
    let* length_str =
      try Ok (String.sub data_str 0 (pred newline_idx)) with _ -> 
        Error (`Msg "Packet.Raw.parse_length: Couldn't extract length")
    in
    let* length =
      int_of_string_opt length_str
      |> result_of_opt "Packet.Raw.parse_length: Length was not an integer"
    in
    let+ data =
      let length_rest =
        String.(length data_str - (length length_str + 1))
      in
      try Ok (String.sub data_str (succ newline_idx) length_rest) with _ -> 
        Error (`Msg "Packet.Raw.parse_length: Couldn't extract length")
    in
    length, data

end

module Client = struct 

  type t = {
    index : int;
    connection_id : string;
    data : string;
  }
  [@@deriving yojson]

  let parse str = str |> Yojson.Safe.from_string |> of_yojson
  
end

module Server = struct 
  
  type t = {
    client_index : int;
    connection_id : string;
  }
  [@@deriving yojson]

  let parse str = str |> Yojson.Safe.from_string |> of_yojson
  
end

(*goto howto parse packet (TCP)
  * readline + try parse packet length info
  * read packets and concat until 'length' is read
  * parse packet data with yojson 
*)

type unfinished = {
  final_length : int;
  buffer : Buffer.t;
}

module Tcp = struct 

  let append ~data unfinished =
    Buffer.add_string unfinished.buffer @@ Cstruct.to_string data;
    if Buffer.length unfinished.buffer = unfinished.final_length then
      `Done (Buffer.contents unfinished.buffer)
    else
      `Unfinished unfinished

end
