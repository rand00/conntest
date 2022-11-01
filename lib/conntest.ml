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

  module Ring = struct

    type 'a t = {
      ring : 'a option array;
      index : int;
    }

    let make n : 'a t =
      let ring = Array.make n None in
      let index = 0 in
      { ring; index }

    let length r = Array.length r.ring

    let push field r =
      let index = succ r.index mod Array.length r.ring in
      r.ring.(index) <- Some field;
      { r with index }

    let wrap_reverse_i r i =
      assert (i >= 0);
      let len = Array.length r.ring in
      if i >= len then None else
        let ci = r.index in
        let i_wrapped = if i <= ci then ci - i else len + (ci - i) in
        Some i_wrapped
    
    let get_previous r i =
      Option.bind (wrap_reverse_i r i) (fun i -> r.ring.(i))
    
    let get_latest r = get_previous r 0

    let map f r =
      let f = function
        | None -> None
        | Some v -> Some (f v)
      in
      { r with ring = Array.map f r.ring }

    let fold_left f acc r =
      let f acc = function
        | None -> acc
        | Some v -> f acc v
      in
      let len = Array.length r.ring in
      let acc = ref acc in
      for i = pred len downto 0 do
        let i = wrap_reverse_i r i |> Option.get in
        acc := f !acc r.ring.(i);
      done;
      !acc

  end

  module Udp_flow = struct
    (* include S.UDP *)

    let ring_size = 5
    let bounded_stream_size = ring_size * 2

    type ring_field = {
      data : Cstruct.t option; (*None => packet is late*)
      packet_index : int;
      (*< goto maybe this field could be avoided, as packet bears this info,
        .. and it's calculated from prev packet otherwise*)
    }

    (*> Note: a stream is used to avoid blocking ringbuffer-handler*)
    type 'a stream = 'a Lwt_stream.t * 'a Lwt_stream.bounded_push

    (*> goto maybe; [sink, source, feeder] could be a single abstraction*)
    type t = {
      is_client : bool;
      sink : ring_field Lwt_mvar.t;
      source : Cstruct.t Mirage_flow.or_eof stream;
      feeder : unit Lwt.t;
      port : int;
      pier : Ipaddr.t;
      pier_port : int;
      conn_id : string;
      (*<goto maybe; this could be avoided, as Conn_map has it as key*)
    }

    let udp_stack = S.udp Sv.stack

    module Conn_map = Map.Make(String)

    (*> Warning: but don't know why you would run two instances of protocol*)
    let conn_map = ref (Conn_map.empty)

    let feed_source ~sink ~source =
      let push_until_packet ~ring ~ring_field ~packet_index_diff =
        let final_packet_index = ring_field.packet_index in
        let rec aux ~ring = function
          | 0 -> ring |> Ring.push ring_field
          | packet_index_diff ->
            let ring_field_not_received =
              let packet_index = final_packet_index - packet_index_diff in
              { packet_index; data = None }
            in
            let ring = ring |> Ring.push ring_field_not_received in
            aux ~ring @@ pred packet_index_diff
        in
        aux ~ring packet_index_diff          
      in
      let rec aux ~last_feeded_packet_index ring =
        Lwt_mvar.take sink >>= fun ring_field ->
        Logs.err (fun m -> m "DEBUG: feed_source: UDP pkt recvd");
        let expected_packet_index =
          match Ring.get_latest ring with
          | None -> 0
          | Some v -> succ v.packet_index
        in
        let packet_index_diff =
          ring_field.packet_index - expected_packet_index
        in
        let ring =
          if packet_index_diff < 0 then (
            (*> Note: this is just easier than calculating index for 'set'
               .. and ring shouldn't be long
            *)
            Logs.err (fun m -> m "DEBUG: feed_source: packet_index_diff < 0");
            ring |> Ring.map (fun ring_field' ->
              if ring_field'.packet_index = ring_field.packet_index then
                ring_field
              else
                ring_field'
            )
            (*< goto here out-of-order also need to be tracked*)
          ) else ((*if packet_index_diff >= 0*)
            if packet_index_diff > 0 then 
              Logs.err (fun m -> m "DEBUG: feed_source: packet_index_diff > 0 (%d)"
                  packet_index_diff
              );
            push_until_packet ~ring ~ring_field ~packet_index_diff
            (*< goto register dropped packet as 'lost' if not seen
              * @idea; return the dropped packet from Ring.insert
            *)
          )
        in
        ring |> Ring.fold_left (fun acc ring_field' ->
          acc >>= fun (last_feeded_packet_index, seen_empty_data) ->
          let seen_empty_data = seen_empty_data || ring_field'.data = None in
          let packet_index_is_newer =
            ring_field'.packet_index > last_feeded_packet_index
          in
          if packet_index_is_newer && not seen_empty_data then
            let data = `Data (Option.get ring_field'.data) in
            Logs.err (fun m -> m "DEBUG: feed_source: pushing packet from RING (bounded-stream elements = %d)"
                ((snd source)#count)
            );
            (snd source)#push data >|= fun () ->
            let last_feeded_packet_index = ring_field'.packet_index in
            last_feeded_packet_index, seen_empty_data
          else
            Lwt.return (last_feeded_packet_index, seen_empty_data)
        ) (Lwt.return (last_feeded_packet_index, false))
        >>= fun (last_feeded_packet_index, _) ->
        aux ~last_feeded_packet_index ring
      in
      aux ~last_feeded_packet_index:(-1) @@ Ring.make ring_size
    
    let listen ~port user_callback =
      let callback ~src ~dst ~src_port data =
        match Packet.Tcp.init ~ignore_data:true data with
        | Ok (`Done (packet, _rest)) ->
          let conn_id = packet.Packet.T.header.connection_id in
          begin match Conn_map.find_opt conn_id !conn_map with
          | None -> 
            let ring_field =
              let data = Some data in
              let packet_index = packet.Packet.T.header.index in
              { data; packet_index }
            in
            let sink = Lwt_mvar.create ring_field in
            let source = Lwt_stream.create_bounded bounded_stream_size in
            let feeder = feed_source ~sink ~source in
            let flow = {
              is_client = false;
              sink;
              source;
              port;
              pier = src;
              pier_port = src_port;
              conn_id;
              feeder;
            } in
            let conn_map' = Conn_map.add conn_id flow !conn_map in
            conn_map := conn_map';
            user_callback flow
          | Some flow ->
            let ring_field =
              let data = Some data in
              let packet_index = packet.Packet.T.header.index in
              { data; packet_index }
            in
            Lwt_mvar.put flow.sink ring_field
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

    module Udp_port = struct 
    
      module PSet = Set.Make(Int)

      let used_ports = ref PSet.empty

      (*goto depends on Random.init done somewhere*)
      let rec allocate () =
        let port = 10_000 + Random.int 50_000 in
        if PSet.mem port !used_ports then
          allocate ()
        else
          let used_ports' = PSet.add port !used_ports in
          used_ports := used_ports';
          port

      let free port =
        let used_ports' = PSet.remove port !used_ports in
        used_ports := used_ports'
      
    end

    let create_connection ~id (pier, pier_port) =
      let sink = Lwt_mvar.create_empty () in
      let source = Lwt_stream.create_bounded bounded_stream_size in
      (*> goto handle that user shouldn't create a server-listening port that
        can be allocated here as well*)
      let port = Udp_port.allocate () in
      let feeder = feed_source ~sink ~source in
      let flow = {
        is_client = true;
        sink;
        source;
        port;
        pier;
        pier_port;
        conn_id = id;
        feeder;
      } in
      let conn_map' = Conn_map.add id flow !conn_map in
      conn_map := conn_map';
      listen ~port (fun _flow -> Lwt.return_unit);
      Lwt_result.return flow

    let read flow =
      Lwt_stream.next (fst flow.source) >|= fun res ->
      Ok res

    let writev flow datas =
      (*> Note: it's important that all cstructs are written at once for ordering*)
      let data = Cstruct.concat datas in
      let src_port = flow.port in
      let dst, dst_port = flow.pier, flow.pier_port in
      (*< spec:
        * listen-case: flow is only given to callback on recv first packet
        * connect-case: flow already has dst + dst_port
      *)
      S.UDP.write ~src_port ~dst ~dst_port udp_stack data
      |> error_to_msg S.UDP.pp_error

    let dst flow = flow.pier, flow.pier_port

    let close flow =
      if flow.is_client then unlisten ~port:flow.port; 
      Lwt.cancel flow.feeder;
      let conn_map' = Conn_map.remove flow.conn_id !conn_map in
      conn_map := conn_map';
      if flow.is_client then Udp_port.free flow.port;
      Lwt.return_unit
    
  end
  
  module Udp = Protocol.Make(Time)(Udp_flow)(O)(struct
    let subproto = `Udp
  end)

end
