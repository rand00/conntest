
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
      try Ok Cstruct.(sub data 0 newline_idx |> to_string) with _ -> 
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

module Header = struct

  type t = {
    index : int;
    connection_id : string;
  }
  [@@deriving yojson]

  let of_string str = str |> Yojson.Safe.from_string |> of_yojson
  let to_string h = h |> to_yojson |> Yojson.Safe.to_string

end

module T = struct 

  type header = Header.t = {
    index : int;
    connection_id : string;
  }

  type t = {
    header : header;
    data : string;
  }

end
include T

let to_string packet =
  let header_str = Header.to_string packet.header in
  let header_len = String.length header_str in
  let data_len = String.length packet.data in
  let packet_str = header_str ^ packet.data in
  String.concat "\n" [
    Int.to_string header_len;
    Int.to_string data_len;
    packet_str;
  ]

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
          |> Header.of_string
          |> Result.map_error (fun s -> `Msg s)
        in
        { unfinished with header = Some header }
      else
        Ok unfinished
    in
    let full_len = unfinished.header_len + unfinished.data_len in
    (*> goto - tcp allows for 'data' to contain some of the next packet too
      * .. so should check for >= full_len
        * and return `Done (packet, buffer-tail)
    *)
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
