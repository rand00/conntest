
(*> goto these are use in Unikernel too - could share*)
let (let*) = Result.bind 
let (let+) x f = Result.map f x 

let result_of_opt msg = function
  | Some v -> Ok v
  | None -> Error (`Msg msg)

module Header = struct

  type meta = [
    | `Normal
    | `Ack
    | `Resend_ack
  ]
  [@@deriving yojson]
  
  type t = {
    index : int;
    connection_id : string;
    meta : meta;
  }
  [@@deriving yojson]

  let of_string str = str |> Yojson.Safe.from_string |> of_yojson
  let to_string h = h |> to_yojson |> Yojson.Safe.to_string

end

module T = struct 

  type header = Header.t = {
    index : int;
    connection_id : string;
    meta : Header.meta;
  }

  type t = {
    header : header;
    data : Cstruct.t;
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
    | None -> Cstruct.length packet.data
    | Some len -> len
  in
  let packet_str = header_str ^ Cstruct.to_string packet.data in
  String.concat "\n" [
    Int.to_string header_len;
    Int.to_string data_len;
    packet_str;
  ]

module CsBuffer = struct

  (*> Note: the idea of this is that ownership of Cstructs is transferred
      by flow - and we don't need to keep these in memory for long*)
  type t = Cstruct.t list

  (*> goto could cache the length on insertion,
    .. though don't expect many cstructs to be here *)
  let length v =
    List.fold_left (fun acc cs -> acc + Cstruct.length cs) 0 v

  let add v cs = cs :: v

  let sub v idx len : Cstruct.t =
    let idx_start = idx in
    let idx_after_end = idx + len in
    let return = Cstruct.create_unsafe len in
    let v_rev = List.rev v in
    let len_add = ref 0 in
    v_rev |> List.iter (fun cs ->
      let len = Cstruct.length cs in
      let cs_idx_start = !len_add in
      let cs_idx_after_end = cs_idx_start + len in
      let common_idx_start = max idx_start cs_idx_start in
      let common_idx_after_end = min idx_after_end cs_idx_after_end in
      if
        idx_start < cs_idx_after_end
        && idx_after_end > cs_idx_start
        && common_idx_start < common_idx_after_end
      then begin
        let cs_local_char_i = common_idx_start - cs_idx_start in
        let return_local_char_i = common_idx_start - idx_start in
        let common_len = common_idx_after_end - common_idx_start in
        Cstruct.blit cs cs_local_char_i return return_local_char_i common_len;
      end;
      len_add := !len_add + len;
    );
    return

  let sub_string v idx len = sub v idx len |> Cstruct.to_string
  
end

type unfinished_with_lengths = {
  header_len : int;
  data_len : int;
  header : header option;
  buffer : CsBuffer.t;
  (*< gomaybe: this could also be a list of cstructs if we own the cstructs*)
}

type unfinished = [
  | `Init of string
  | `With_lengths of unfinished_with_lengths
]

module Tcp = struct 

  let rec append ~data ?(ignore_data=false) (unfinished:unfinished) =
    match unfinished with
    | `Init str -> append_init_aux ~str ~data
    | `With_lengths unfinished ->
      let buffer = CsBuffer.add unfinished.buffer data in
      let unfinished = { unfinished with buffer } in
      let len_buffer = CsBuffer.length unfinished.buffer in
      let+ unfinished =
        begin match unfinished.header with
          | Some _ -> Ok unfinished
          | None -> 
            if len_buffer >= unfinished.header_len then (
              let+ header =
                CsBuffer.sub_string unfinished.buffer 0 unfinished.header_len
                |> Header.of_string
                |> Result.map_error (fun s -> `Msg s)
              in
              { unfinished with header = Some header }
            ) else
              Ok unfinished
        end
      in
      let full_len = unfinished.header_len + unfinished.data_len in
      if len_buffer >= full_len then
        let header = Option.get unfinished.header in
        let data =
          if ignore_data then Cstruct.empty else 
            CsBuffer.sub unfinished.buffer
              unfinished.header_len
              unfinished.data_len
        in
        let more_data =
          if len_buffer > full_len then (
            let rest =
              CsBuffer.sub unfinished.buffer
                full_len
                (len_buffer - full_len)
            in
            Some rest
          ) else None
        in
        `Done ({ header; data }, more_data)
      else
        `Unfinished (`With_lengths unfinished)

  and append_init_aux ~str ~data =
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
      let buffer = [Cstruct.of_string rest] in
      let unfinished = { header_len; data_len; header = None; buffer } in
      append ~data:Cstruct.empty @@ `With_lengths unfinished

  let init ?(ignore_data=false) data = append ~data ~ignore_data @@ `Init ""
    
end
