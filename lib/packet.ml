
(*> goto these are use in Unikernel too - could share*)
let (let*) = Result.bind 
let (let+) x f = Result.map f x 

let result_of_opt msg = function
  | Some v -> Ok v
  | None -> Error (`Msg msg)

module Raw = struct

  let find_index c data =
    let len = Cstruct.length data in
    let rec aux i =
      if i >= len then None else 
      if Char.equal c (Cstruct.get data i) then
        Some i
      else
        aux @@ succ i
    in
    aux 0
  
  let parse_length data =
    let* newline_idx =
      data
      |> find_index '\n'
      |> result_of_opt "Packet.Raw.parse_length: No newline found"
    in
    let* length_str =
      try Ok Cstruct.(sub data 0 (pred newline_idx) |> to_string) with _ -> 
        Error (`Msg "Packet.Raw.parse_length: Couldn't extract length")
    in
    let* length =
      int_of_string_opt length_str
      |> result_of_opt "Packet.Raw.parse_length: Length was not an integer"
    in
    let+ data =
      let length_rest =
        Cstruct.length data - (newline_idx + 1)
      in
      try Ok (Cstruct.sub data (newline_idx + 1) length_rest) with _ -> 
        Error (`Msg "Packet.Raw.parse_length: Couldn't extract length")
    in
    length, data

end

module T = struct 

  type header = {
    index : int;
    connection_id : string;
  }
  [@@deriving yojson]

  type t = {
    header : header;
    data : string;
  }

end
include T

let parse_header str = str |> Yojson.Safe.from_string |> header_of_yojson

type unfinished = {
  header_len : int;
  data_len : int;
  header : header option;
  buffer : Buffer.t;
  (*< gomaybe: this could also be a list of cstructs if we own the cstructs*)
}

module Tcp = struct 

  let append ~data unfinished =
    Buffer.add_string unfinished.buffer @@ Cstruct.to_string data;
    let+ unfinished =
      if Buffer.length unfinished.buffer >= unfinished.header_len then
        let+ header =
          Buffer.sub unfinished.buffer 0 unfinished.header_len
          |> parse_header
          |> Result.map_error (fun s -> `Msg s)
        in
        { unfinished with header = Some header }
      else
        Ok unfinished
    in
    let full_len = unfinished.header_len + unfinished.data_len in
    if Buffer.length unfinished.buffer = full_len then
      let header = Option.get unfinished.header in
      let data =
        Buffer.sub unfinished.buffer
          unfinished.header_len
          unfinished.data_len
      in
      `Done { header; data }
    else
      `Unfinished unfinished

  let init data =
    let* header_len, data = Raw.parse_length data in
    let* data_len, data = Raw.parse_length data in
    let buffer = Buffer.create 512 in
    let unfinished = { header_len; data_len; header = None; buffer } in
    append ~data unfinished
    
end
