
(*> goto these are use in Unikernel too - could share*)
let (let*) = Result.bind 
let (let+) x f = Result.map f x 

let result_of_opt msg = function
  | Some v -> Ok v
  | None -> Error (`Msg msg)

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

let to_cstructs ~header ~data =
  let header_str = Header.to_string header in
  let header_len = String.length header_str in
  let data_len = Cstruct.length data in
  let lengths_str = Fmt.str "%d\n%d\n" header_len data_len in 
  [
    lengths_str |> Cstruct.of_string;
    header_str |> Cstruct.of_string;
    data
  ]

let to_string ?override_data_len packet =
  let header_str = Header.to_string packet.header in
  let header_len = String.length header_str in
  let data_len = match override_data_len with
    | None -> String.length packet.data
    | Some len -> len
  in
  let packet_str = header_str ^ packet.data in
  String.concat "\n" [
    Int.to_string header_len;
    Int.to_string data_len;
    packet_str;
  ]

type unfinished_with_lengths = {
  header_len : int;
  data_len : int;
  header : header option;
  buffer : Buffer.t;
  (*< gomaybe: this could also be a list of cstructs if we own the cstructs*)
}

type unfinished = [
  | `Init of string
  | `With_lengths of unfinished_with_lengths
]

module Tcp = struct 

  (*> goto separate sections out from this function for readability*)
  let rec append ~data (unfinished:unfinished) =
    match unfinished with
    | `Init str ->
      begin
        let str = str ^ Cstruct.to_string data in
        let newline_idxs = begin
          let (let*) = Option.bind in
          let* idx_1 = String.index_opt str '\n' in
          let* idx_2 =
            try String.index_from_opt str (succ idx_1) '\n' with
            | Invalid_argument _ -> None
          in
          Some (idx_1, idx_2)
        end in
        match newline_idxs with 
        | None -> Ok (`Unfinished (`Init str))
        | Some (idx_1, idx_2) ->
          let* header_len =
            let header_len_str = String.sub str 0 idx_1 in
            header_len_str
            |> int_of_string_opt 
            |> result_of_opt (
              Fmt.str "Packet.Tcp.append: Header-length was not an integer: '%s'"
                header_len_str
            )
          in
          let* data_len =
            let idx = succ idx_1 in
            let len = idx_2 - idx in
            let data_len_str = String.sub str idx len in
            data_len_str
            |> int_of_string_opt 
            |> result_of_opt (
              Fmt.str "Packet.Tcp.append: Data-length was not an integer: '%s'"
                data_len_str
            )
          in
          let rest =
            String.(sub str (succ idx_2) (length str - (succ idx_2))) in
          let buffer = Buffer.create 512 in
          Buffer.add_string buffer rest;
          let unfinished = { header_len; data_len; header = None; buffer } in
          append ~data:Cstruct.empty @@ `With_lengths unfinished
      end
    | `With_lengths unfinished -> 
      Buffer.add_string unfinished.buffer @@ Cstruct.to_string data;
      let+ unfinished =
        begin match unfinished.header with
          | Some _ -> Ok unfinished
          | None -> 
            if Buffer.length unfinished.buffer >= unfinished.header_len then (
              let+ header =
                Buffer.sub unfinished.buffer 0 unfinished.header_len
                |> Header.of_string
                |> Result.map_error (fun s -> `Msg s)
              in
              { unfinished with header = Some header }
            ) else
              Ok unfinished
        end
      in
      let full_len = unfinished.header_len + unfinished.data_len in
      if Buffer.length unfinished.buffer >= full_len then
        let header = Option.get unfinished.header in
        let data =
          Buffer.sub unfinished.buffer
            unfinished.header_len
            unfinished.data_len
        in
        let more_data =
          if Buffer.length unfinished.buffer > full_len then (
            let rest =
              Buffer.sub unfinished.buffer
                full_len
                (Buffer.length unfinished.buffer - full_len)
            in
            Some (Cstruct.of_string rest)
          ) else None
        in
        `Done ({ header; data }, more_data)
      else
        `Unfinished (`With_lengths unfinished)

  let init data = append ~data @@ `Init ""
    
end
