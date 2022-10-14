
(*goto rename this module to Ui ? - Output can mean many things.. *)

type connect_tcp_error = [
  | `Eof
  | `Msg of string
  | `Read of (Tcpip.Tcp.error option * string (*message*))
  (*< Note: the option is because of private variants*)
]

module type S = sig

  module Listen : sig

    module Tcp : sig
      val registered_listener : port:int -> unit
      val new_connection : conn_id:string -> ip:Ipaddr.t -> port:int -> unit
      val closing_connection : conn_id:string -> ip:Ipaddr.t -> port:int -> unit
      (*> goto all errors should be explicit, so user can get all the info wanted
        .. and notty interface needs error values instead of just a string 
      *)
      val error : conn_id:string  -> ip:Ipaddr.t -> port:int -> err:string -> unit
      val received_packet :
        conn_id:string -> ip:Ipaddr.t -> port:int
        -> header:Packet.header -> protocol:Protocol.t option -> unit
      val sent_packet :
        conn_id:string -> ip:Ipaddr.t -> port:int
        -> header:Packet.header -> protocol:Protocol.t option -> unit
    end

    module Udp : sig
      val data : ip:Ipaddr.t -> port:int -> data:Cstruct.t -> unit
      val registered_listener : port:int -> unit
    end
    
  end

  module Connect : sig

    module Tcp : sig

      val connecting : conn_id:string -> ip:Ipaddr.t -> port:int -> unit
      val connected : conn_id:string -> ip:Ipaddr.t -> port:int -> unit
      (*> goto remove data arg here? - maybe useful for debugging*)
      val writing : conn_id:string -> ip:Ipaddr.t -> port:int -> data:Cstruct.t -> unit
      val sent_packet :
        conn_id:string -> ip:Ipaddr.t -> port:int
        -> header:Packet.header -> protocol:Protocol.t option -> unit
      val received_packet :
        conn_id:string -> ip:Ipaddr.t -> port:int
        -> header:Packet.header -> protocol:Protocol.t option -> unit
      (*> goto all errors should be explicit, so user can get all the info wanted
        .. and notty interface needs error values instead of just a string 
      *)
      val error : conn_id:string -> ip:Ipaddr.t -> port:int
        -> err:connect_tcp_error  
        -> unit
      val error_connection :
        conn_id:string -> ip:Ipaddr.t -> port:int -> err:string -> unit
      (* val error_writing : conn_id:string -> ip:Ipaddr.t -> port:int
       *   -> err:(Tcpip.Tcp.write_error option) -> msg:string
       *   -> unit
       * val error_reading : conn_id:string -> ip:Ipaddr.t -> port:int
       *   -> err:connect_tcp_error 
       *   -> unit *)
      (*> goto rename to closing_connection for uniformity*)
      val closing_flow : conn_id:string -> ip:Ipaddr.t -> port:int -> unit
      (*> goto remove?*)
      val closed_flow : conn_id:string -> ip:Ipaddr.t -> port:int -> unit
          
    end

    module Udp : sig

      val writing : ip:Ipaddr.t -> port:int -> data:string -> unit

    end
    
  end

end

module Log_stdout () : S = struct

  let src = Logs.Src.create "conntest" ~doc:"conntest events"
  module Log = (val Logs.src_log src : Logs.LOG)

  let preview_big_string str =
    if String.length str > 50 then 
      Stringext.take str 50 ^ "..."
    else str
  
  module Listen = struct

    module Tcp = struct
      
      let new_connection ~conn_id:_ ~ip ~port =
        Log.info (fun m ->
          m "new tcp connection from %s:%d"
            (Ipaddr.to_string ip) port)

      let closing_connection ~conn_id:_ ~ip ~port =
        Log.info (fun f -> f "closing tcp connection to %s:%d"
            (Ipaddr.to_string ip) port
        )

      let error ~conn_id:_ ~ip ~port ~err =
        Log.warn (fun f ->
          f "error reading data from tcp connection %s:%d:\n%s"
            (Ipaddr.to_string ip) port
            err)

      let registered_listener ~port =
        Log.info (fun f -> f "registered tcp listener on port %d" port)

      let received_packet ~conn_id:_ ~ip ~port ~header ~protocol =
        let open Packet.T in
        Log.info (fun f ->
          f "got packet from %s:%d:\n---- header:\n%s\n---- protocol:\n%s"
            (Ipaddr.to_string ip) port
            (header |> Packet.Header.to_string)
            (protocol
             |> Option.map Protocol.to_string
             |> Option.value ~default:"None")
        )

      let sent_packet ~conn_id:_ ~ip ~port ~header ~protocol =
        let open Packet.T in
        Log.info (fun f ->
          f "sent packet to %s:%d:\n---- header:\n%s\n---- protocol:\n%s"
            (Ipaddr.to_string ip) port
            (header |> Packet.Header.to_string)
            (protocol
             |> Option.map Protocol.to_string
             |> Option.value ~default:"None")
        )

    end

    module Udp = struct

      let data ~ip ~port ~data =
        Log.info (fun f ->
          f "read %d bytes from %s:%d:\n%s"
            (Cstruct.length data)
            (Ipaddr.to_string ip) port
            (Cstruct.to_string data))

      let registered_listener ~port =
        Log.info (fun f -> f "registered udp listener on port %d" port)

    end

  end

  module Connect = struct

    module Tcp = struct

      let connecting ~conn_id:_ ~ip ~port =
        Log.info (fun m ->
          m "connecting via tcp to %s:%d"
            (Ipaddr.to_string ip) port)

      let connected ~conn_id:_ ~ip ~port =
        Log.info (fun m ->
          m "connected via tcp to %s:%d"
            (Ipaddr.to_string ip) port)

      let writing ~conn_id:_ ~ip ~port ~data =
        Log.info (fun m ->
          m "writing via tcp to %s:%d" (Ipaddr.to_string ip) port
        ) 

      let error_connection ~conn_id:_ ~ip ~port ~err =
        Log.warn (fun f ->
          f "error connecting via tcp to %s:%d:\n%s"
            (Ipaddr.to_string ip) port
            err)

      let error ~conn_id:_ ~ip ~port ~err =
        match err with
        | `Msg msg 
        | `Read (_, msg) -> 
          Log.warn (fun f ->
            f "error reading via tcp from %s:%d:\n%s"
              (Ipaddr.to_string ip) port msg
          )
        | `Eof ->
          Log.warn (fun f ->
            f "error reading via tcp from %s:%d:\n%s"
              (Ipaddr.to_string ip) port "End of file"
          )

      let received_packet ~conn_id:_ ~ip ~port ~header ~protocol =
        let open Packet.T in
        Log.info (fun f ->
          f "got packet from %s:%d:\n---- header:\n%s\n---- protocol:\n%s"
            (Ipaddr.to_string ip) port
            (header |> Packet.Header.to_string)
            (protocol
             |> Option.map Protocol.to_string
             |> Option.value ~default:"None")
        )

      let sent_packet ~conn_id:_ ~ip ~port ~header ~protocol =
        let open Packet.T in
        Log.info (fun f ->
          f "sent packet to %s:%d:\n---- header:\n%s\n---- protocol:\n%s"
            (Ipaddr.to_string ip) port
            (header |> Packet.Header.to_string)
            (protocol
             |> Option.map Protocol.to_string
             |> Option.value ~default:"None")
        )
      
      let closing_flow ~conn_id:_ ~ip ~port =
        Log.info (fun m ->
          m "closing tcp flow to %s:%d" (Ipaddr.to_string ip) port)
        
      let closed_flow ~conn_id:_ ~ip ~port =
        Log.info (fun m ->
          m "closed tcp flow to %s:%d" (Ipaddr.to_string ip) port)

    end

    module Udp = struct

      let writing ~ip ~port ~data =
        Log.info (fun m ->
          m "writing via udp to %s on port %d: %s"
            (Ipaddr.to_string ip) port data)

    end

  end

end

module type NOTTY_UI_ARGS = sig

  val name : string
  (* val term_dimensions : int * int  *)

end

(*goto move these types out to other file?*)
module Eq = struct

  let never _ _ = false
  let always _ _ = true 

end

(*goto move these types out to other file?*)
module Tuple = struct

  let mk2 v0 v1 = v0, v1
  let mk3 v0 v1 v2 = v0, v1, v2
  let mk4 v0 v1 v2 v3 = v0, v1, v2, v3
  let mk5 v0 v1 v2 v3 v4 = v0, v1, v2, v3, v4

end

module Conn_id = struct

  module T = struct

    type t = string

  end
  include T

  let compare = String.compare

end

module Conn_id_map = Map.Make(Conn_id)

module Pier = struct

  module T = struct 

    type t = {
      ip : Ipaddr.t;
      port : int;
      conn_id : Conn_id.t;
      (*< Note: this is the local id
        - the conn-id sent via header is for sending back*)
    }

  end
  include T

end

module Connection = struct

  module T = struct 

    type id = Uuidm.t

    type t = {
      typ : [ `Client | `Server ];
      pier : Pier.t;
      pier_name : string option;
      protocol : [ `Tcp | `Udp ];
      start_time : Int64.t; (*arbitrary startpoint in nanoseconds*)
      error : bool;
      sent_packets : int;
      received_packets : int;
      retries : int option;
      latency : Int64.t option; (*ns*)
      bandwidth : float option; (*MB/sec*)
      packet_size : int option; (*bytes*)
      (* lost_packets : int option; *)
      (* out_of_order_packets : int option; *)
    }

  end
  include T

  let make ~typ ~protocol ~start_time ~pier = {
    typ;
    protocol;
    start_time;
    pier;
    error = false;
    pier_name = None;
    sent_packets = 0;
    received_packets = 0;
    retries = None;
    latency = None;
    bandwidth = None;
    packet_size = None;
  }

end
open Connection.T
open Pier.T

(*> goto make this fit S signature, or inner module?*)
module Notty_ui
    (Time : Mirage_time.S)
    (Clock : Mirage_clock.MCLOCK)
    (Args : NOTTY_UI_ARGS)
= struct

  open Lwt_react
  open Lwt.Infix 

  module Tick = struct

    let fps = 60. 
    let fps_sleep_ns = 1e9 /. fps 

    let e, eupd = E.create ()

    let s = S.hold 0 e

    let loop_feed () =
      let rec aux i =
        eupd i;
        Time.sleep_ns @@ Int64.of_float fps_sleep_ns >>= fun () ->
        aux @@ succ i
      in
      aux 0
    
  end

  let init () = Tick.loop_feed ()

  let (name_s : string S.t), name_supd = S.create Args.name

  (* let (term_dimensions_s : (int * int) S.t), term_dimensions_supd =
   *   S.create Args.term_dimensions *)

  (*goto maybe put S on via mli*)
  module Input_event (* : S *) = struct

    module Listen = struct

      module Tcp = struct

        type event = [
          | `New_connection of Pier.t
          | `Closing_connection of Pier.t
          | `Error of (Pier.t * string)
          | `Recv_packet of (Pier.t * Protocol.t option)
          | `Sent_packet of (Pier.t * Protocol.t option)
        ]
        
        let (e : event E.t), eupd = E.create ()

        let new_connection ~conn_id ~ip ~port =
          eupd @@ `New_connection {ip; port; conn_id}

        let closing_connection ~conn_id ~ip ~port =
          eupd @@ `Closing_connection {ip; port; conn_id}

        let error ~conn_id ~ip ~port ~err =
          eupd @@ `Error ({ip; port; conn_id}, (err : string))

        let registered_listener ~port = () 

        (*> Note: there is also a remote conn-id in header*)
        let received_packet ~conn_id ~ip ~port ~header ~protocol =
          eupd @@ `Recv_packet ({ip; port; conn_id}, protocol)

        let sent_packet ~conn_id ~ip ~port ~header ~protocol =
          eupd @@ `Sent_packet ({ip; port; conn_id}, protocol)
        
      end

      module Udp = struct

        let data ~ip ~port ~data =
          () (*goto*)

        let registered_listener ~port =
          () (*goto*)

      end

    end

    module Connect = struct

      module Tcp = struct

        (*goto make (sub)type common for server/client?*)
        type event = [
          | `New_connection of Pier.t
          | `Closing_connection of Pier.t
          (* | `Error of (Pier.t * string) *)
          | `Recv_packet of (Pier.t * Protocol.t option)
          | `Sent_packet of (Pier.t * Protocol.t option)
        ]
        
        let (e : event E.t), eupd = E.create ()

        let connecting ~conn_id:_ ~ip ~port =
          () (*goto*)

        let connected ~conn_id ~ip ~port =
          eupd @@ `New_connection {ip; port; conn_id}

        let writing ~conn_id:_ ~ip ~port ~data =
          () (*goto*)

        let error_connection ~conn_id:_ ~ip ~port ~err =
          () (*goto*)

        let error ~conn_id:_ ~ip ~port ~err =
          () (*goto*)

        let received_packet ~conn_id ~ip ~port ~header ~protocol  = 
          eupd @@ `Recv_packet ({ip; port; conn_id}, protocol)

        let sent_packet ~conn_id ~ip ~port ~header ~protocol = 
          eupd @@ `Sent_packet ({ip; port; conn_id}, protocol)

        let closing_flow ~conn_id ~ip ~port =
          eupd @@ `Closing_connection {ip; port; conn_id}

        let closed_flow ~conn_id:_ ~ip ~port =
          () (*goto*)

      end

      module Udp = struct

        let writing ~ip ~port ~data =
          () (*goto*)

      end

    end

  end

  module Data = struct

    type latency_data = {
      start_time : Int64.t; (*nanoseconds*)
      latency_snapshot : Int64.t option; (*nanoseconds*)
    }

    type bandwidth_data = {
      start_time : Int64.t; (*nanoseconds*)
      packet_size : int;
      bandwidth_snapshot : float option; (*MB/sec*)
      i : int;
    }

    let sec_of_ns ns = Int64.to_float ns /. 1e9
                         
    module Calc = struct

      (*> goto should maybe remove state when connection closes*)
      let latency acc (pier, event) =
        let elapsed_ns = Clock.elapsed_ns () in
        match event with
        | `Roundtrip_start -> 
          Conn_id_map.update pier.conn_id (function
            | None ->
              let start_time = elapsed_ns in 
              let latency_snapshot = None in
              Some { start_time; latency_snapshot }
            | Some (data:latency_data) ->
              let start_time = elapsed_ns in 
              Some { data with start_time }
          ) acc
        | `Roundtrip_end ->
          Conn_id_map.update pier.conn_id (function
            | None -> None (*shouldn't happen*)
            | Some (data:latency_data) ->
              let roundtrip_ns = Int64.sub elapsed_ns data.start_time in
              let half_roundtrip_ns =
                Int64.to_float roundtrip_ns /. 2.
                |> Int64.of_float
              in
              let latency_snapshot = Some half_roundtrip_ns in
              Some { data with latency_snapshot } 
          ) acc
      
      (*> goto should maybe remove state when connection closes*)
      let bandwidth acc event =
        let elapsed_ns = Clock.elapsed_ns () in
        match event with
        | `Init (pier, packet_size) ->
          let start_time = elapsed_ns in 
          Conn_id_map.update pier.conn_id (function
            | None ->
              let i = 0
              and bandwidth_snapshot = None in
              Some { start_time; packet_size; i; bandwidth_snapshot }
            | Some data ->
              let i = 0 in
              Some { data with start_time; packet_size; i } 
          ) acc
        | `Packet pier ->
          Conn_id_map.update pier.conn_id (function
            | None -> None (*shouldn't happen*)
            | Some data ->
              let i = succ data.i in
              let bandwidth_snapshot =
                let mbytes_xfr = float (data.packet_size * i) /. 1e6 in
                let secs = sec_of_ns Int64.(sub elapsed_ns data.start_time) in
                Some (mbytes_xfr /. secs)
              in
              Some { data with i; bandwidth_snapshot } 
          ) acc

      let connection_state ~typ ~protocol
          acc (event, (latencies, bandwidths))
        =
        let elapsed_ns = Clock.elapsed_ns () in
        match event with
        | `New_connection pier ->
          let start_time = elapsed_ns in 
          let conn = Connection.make ~typ ~protocol ~start_time ~pier in
          acc |> Conn_id_map.add pier.conn_id conn
        | `Closing_connection pier ->
          Conn_id_map.remove pier.conn_id acc
        | `Error (pier, _err) ->
          Conn_id_map.update pier.conn_id (function
            | None -> None
            | Some conn -> Some { conn with error = true }
          ) acc
        | `Recv_packet (pier, protocol) ->
          Conn_id_map.update pier.conn_id (function
            | None -> None (*shouldn't happen*)
            | Some conn ->
              let received_packets = succ conn.received_packets in
              let conn = { conn with received_packets } in
              let conn = match protocol with
                | Some (`Hello info) ->
                  let pier_name = Some info.Protocol.T.name in
                  { conn with pier_name }
                | _ -> conn
              in
              (*> goto optimization: could avoid these updates on general packets,
                  .. and only on certain events instead*)
              let conn =
                match Conn_id_map.find_opt pier.conn_id latencies with
                | None -> conn
                | Some latency_data ->
                  let latency = latency_data.latency_snapshot in
                  { conn with latency } 
              in
              let conn =
                match Conn_id_map.find_opt pier.conn_id bandwidths with
                | None -> conn
                | Some bwm_data ->
                  let packet_size = Some bwm_data.packet_size in
                  let bandwidth = bwm_data.bandwidth_snapshot
                  in
                  { conn with packet_size; bandwidth }
              in
              Some conn
          ) acc
        | `Sent_packet (pier, protocol) ->
          Conn_id_map.update pier.conn_id (function
            | None -> None (*shouldn't happen*)
            | Some conn ->
              let sent_packets = succ conn.sent_packets in
              Some { conn with sent_packets }
          ) acc

    end
    
    module Tcp_server = struct

      let input_e = Input_event.Listen.Tcp.e 

      let latencies_e =
        let open Protocol.T in
        let input_e =
          input_e
          |> E.fmap (function
            | `Sent_packet (pier, Some (`Latency `Pong)) ->
              Some (pier, `Roundtrip_start)
            | `Recv_packet (pier, Some (`Latency `Pong)) ->
              Some (pier, `Roundtrip_end)
            | _ -> None
          )
        in
        input_e
        |> E.fold Calc.latency Conn_id_map.empty

      let latencies_s = S.hold ~eq:Eq.never Conn_id_map.empty latencies_e

      let bandwidths_e =
        let open Protocol.T in
        let input_e =
          input_e
          |> E.fmap (function
            | `Recv_packet (pier, Some (`Bandwidth (
              { direction = `Up; _} as b))) ->
              Some (`Init (pier, b.packet_size))
            | `Recv_packet (pier, None) ->
              Some (`Packet pier)
            | _ -> None
          )
        in
        input_e
        |> E.fold Calc.bandwidth Conn_id_map.empty

      let bandwidths_s = S.hold ~eq:Eq.never Conn_id_map.empty bandwidths_e

      let connections_e =
        let typ = `Server in
        let protocol = `Tcp in
        let sampled_s =
          S.l2 ~eq:Eq.never Tuple.mk2
            latencies_s
            bandwidths_s
        in
        S.sample Tuple.mk2 input_e sampled_s
        |> E.fold (Calc.connection_state ~typ ~protocol) Conn_id_map.empty

      let connections_s = S.hold ~eq:Eq.never Conn_id_map.empty connections_e

    end

    module Tcp_client = struct

      let input_e = Input_event.Connect.Tcp.e 
      
      let latencies_e =
        let open Protocol.T in
        let input_e =
          input_e
          |> E.fmap (function
            | `Sent_packet (pier, Some (`Latency `Ping)) ->
              Some (pier, `Roundtrip_start)
            | `Recv_packet (pier, Some (`Latency `Pong)) ->
              Some (pier, `Roundtrip_end)
            | _ -> None
          )
        in
        input_e 
        |> E.fold Calc.latency Conn_id_map.empty

      let latencies_s = S.hold ~eq:Eq.never Conn_id_map.empty latencies_e

      let bandwidths_e =
        let open Protocol.T in
        let input_e =
          input_e
          |> E.fmap (function
            | `Sent_packet (pier, Some (`Bandwidth (
              {direction = `Down; _} as b))) ->
              Some (`Init (pier, b.packet_size))
            | `Recv_packet (pier, None) ->
              Some (`Packet pier)
            | _ -> None
          )
        in
        input_e
        |> E.fold Calc.bandwidth Conn_id_map.empty

      let bandwidths_s = S.hold ~eq:Eq.never Conn_id_map.empty bandwidths_e

      let connections_e =
        let typ = `Client in
        let protocol = `Tcp in
        let sampled_s =
          S.l2 ~eq:Eq.never Tuple.mk2
            latencies_s
            bandwidths_s
        in
        S.sample Tuple.mk2 input_e sampled_s
        |> E.fold (Calc.connection_state ~typ ~protocol) Conn_id_map.empty

      let connections_s = S.hold ~eq:Eq.never Conn_id_map.empty connections_e

    end
        
  end
  
  module Render = struct 

    open Notty 

    let render_table elapsed_ns conn =
      let sep_i = I.(string " | " <-> string " | ") in
      let make_column ?(no_sep=false) title data_i =
        let title_i = I.string title in
        let col_i = I.(title_i <-> data_i) in
        if no_sep then col_i else I.(sep_i <|> col_i)
      in
      let protocol_i = make_column ~no_sep:true "prot" @@
        match conn.protocol with
        | `Tcp -> I.string "TCP"
        | `Udp -> I.string "UDP"
      and sent_packages_i =
        make_column "#sent" @@ I.strf "%d" conn.sent_packets
      and recv_packages_i =
        make_column "#recv" @@ I.strf "%d" conn.received_packets
      and uptime_i =
        let uptime_str =
          let ns = Int64.sub elapsed_ns conn.start_time in
          let s = Duration.to_sec ns mod 60 in
          let m = Duration.to_min ns mod 60 in
          let h = Duration.to_hour ns mod 24 in
          if h = 0 && m = 0 then
            Fmt.str "%ds" s
          else if h = 0 then 
            Fmt.str "%dm%ds" m s
          else           
            Fmt.str "%dh%dm%ds" h m s
        in
        make_column "uptime" @@ I.string uptime_str
      and latency_i =
        let latency_str =
          match conn.latency with
          | None -> "N/A"
          | Some latency ->
            let latency = Int64.to_float latency in
            if latency > 1e9 then
              Fmt.str "%.1fs" @@ latency /. 1e9
            else if latency > 1e6 then
              Fmt.str "%.0fms" @@ latency /. 1e6
            else if latency > 1e3 then
              Fmt.str "%.0fμs" @@ latency /. 1e3
            else 
              Fmt.str "%.0fns" latency
        in
        make_column "latnc" @@ I.string latency_str
      and packet_size_i =
        let str =
          match conn.packet_size with
          | None -> "N/A"
          | Some v ->
            let v = float v in
            if v > 1e6 then
              Fmt.str "%.0fMB" @@ v /. 1e6
            else if v > 1e3 then
              Fmt.str "%.0fKB" @@ v /. 1e3
            else 
              Fmt.str "%.0fB" v
        in
        make_column "p-size" @@ I.string str
      and bandwidth_i =
        let bandwidth_str =
          match conn.bandwidth with
          | None -> "N/A"
          | Some b -> 
            if b >= 10. then 
              Fmt.str "%.0fMB/s" b
            else
              Fmt.str "%.1fMB/s" b
        in
        make_column "bndwdth" @@ I.string bandwidth_str
      in
      [
        [
          protocol_i;
          uptime_i;
          sent_packages_i;
          recv_packages_i;
          latency_i;
          bandwidth_i;
          packet_size_i;
        ];
      ]
      |> List.flatten
      |> I.hcat 

    let render_pier ~width conn =
      let line_i =
        let pier_name = Option.value conn.pier_name ~default:"N/A" in
        [
          I.string "to:";
          I.string ~attr:A.(fg red) pier_name;
          I.strf " | ip:%a | port:%d"
            Ipaddr.pp conn.pier.Pier.ip
            conn.pier.Pier.port
        ]
        |> I.hcat
      in
      line_i
      |> I.hsnap ~align:`Middle width
      |> I.vpad 0 1

    let render_hsep ~width ~attr = I.string ~attr (String.make width '-') 
    
    let render_conn ~width ~elapsed_ns (_id, conn) =
      let pier_i = render_pier ~width conn in
      let table_i = render_table elapsed_ns conn in
      I.(pier_i <-> table_i)
    
    let render_connections
        (prev, tick, this_name, server_conns, client_conns)
      =
      let prev_image, prev_tick, prev_max_conn_width = prev in
      if tick = prev_tick then prev else 
        let width = prev_max_conn_width in
        let elapsed_ns = Clock.elapsed_ns () in
        let server_conns = Conn_id_map.bindings server_conns
        and client_conns = Conn_id_map.bindings client_conns
        in
        let render_conn = render_conn ~width ~elapsed_ns in
        let client_conn_images = client_conns |> List.map render_conn
        and server_conn_images = server_conns |> List.map render_conn 
        in
        let max_conn_width =
          (client_conn_images @ server_conn_images)
          |> List.fold_left (fun acc image ->
            Int.max (I.width image) acc
          ) 0
        in
        let sep_i =
          let attr = A.empty in
          render_hsep ~width:(width + 1) ~attr
        in
        let client_conn_images =
          client_conn_images |> List.map (fun conn_image ->
            I.(conn_image <-> sep_i)
          ) 
        and server_conn_images =
          server_conn_images |> List.map (fun conn_image ->
            I.(conn_image <-> sep_i)
          ) 
        in
        let name_i =
          this_name
          |> I.string ~attr:A.(fg red)
          |> I.hsnap ~align:`Middle width
        in
        let header_attr = A.(fg red) in
        let client_conns_name_i =
          let attr = header_attr in
          [
            I.string ~attr " As client " |> I.hsnap ~align:`Middle width;
            render_hsep ~width:(width + 1) ~attr;
          ]
          |> I.zcat
        in
        let server_conns_name_i =
          let attr = header_attr in
          [
            I.string ~attr " As server " |> I.hsnap ~align:`Middle width;
            render_hsep ~width:(width + 1) ~attr;
          ]
          |> I.zcat
        in
        let make_section name_i = function
          | [] -> []
          | conn_is -> I.void 0 1 :: name_i :: conn_is 
        in
        let image =
          [
            [name_i];
            make_section client_conns_name_i client_conn_images;
            make_section server_conns_name_i server_conn_images;
          ]
          |> List.flatten
          |> I.vcat
        in
        image, tick, max_conn_width
    
    let image_s =
      let define prev_s = 
        let s =
          S.l5 ~eq:Eq.never Tuple.mk5
            prev_s
            Tick.s
            name_s
            Data.Tcp_server.connections_s
            Data.Tcp_client.connections_s
          |> S.map ~eq:Eq.never render_connections
        in
        let s' = s |> S.map (fun (v, _, _) -> v) in
        s, s'
      in
      S.fix (I.empty, 0 (*tick*), 0(*width*)) define

    let image_e =
      image_s |> S.changes

    let dimensions_s =
      image_s |> S.map (fun image -> I.width image, I.height image)

    let dimensions_e = S.changes dimensions_s
    
  end

  let image_e = Render.image_e

end
