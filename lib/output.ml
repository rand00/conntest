
(*goto rename this module to Ui ? - Output can mean many things.. *)

type connect_tcp_read_error = [
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
      (*> goto all errors should be explicit, so user can get all the info wanted
        .. and notty interface needs error values instead of just a string 
      *)
      val sent_packet :
        conn_id:string -> ip:Ipaddr.t -> port:int
        -> header:Packet.header -> protocol:Protocol.t option -> unit
      val received_packet :
        conn_id:string -> ip:Ipaddr.t -> port:int
        -> header:Packet.header -> protocol:Protocol.t option -> unit
      val error_connection :
        conn_id:string -> ip:Ipaddr.t -> port:int -> err:string -> unit
      val error_writing : conn_id:string -> ip:Ipaddr.t -> port:int
        -> err:(Tcpip.Tcp.write_error option) -> msg:string
        -> unit
      val error_reading : conn_id:string -> ip:Ipaddr.t -> port:int
        -> err:connect_tcp_read_error 
        -> unit
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

      let error_writing ~conn_id:_ ~ip ~port ~err:_ ~msg =
        Log.warn (fun f ->
          f "error writing via tcp to %s:%d:\n%s"
            (Ipaddr.to_string ip) port msg
        )

      let error_reading ~conn_id:_ ~ip ~port ~err =
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
  val term_dimensions : int * int 

end

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

    let loop_feed () =
      let rec aux i =
        eupd i;
        Time.sleep_ns @@ Int64.of_float fps_sleep_ns >>= fun () ->
        aux @@ succ i
      in
      aux 0

  end

  let init () = Tick.loop_feed ()

  (*goto move these types out to other file*)

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
        latency : Duration.t option;
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

  let (name_s : string S.t), name_supd = S.create Args.name

  let (term_dimensions_s : (int * int) S.t), term_dimensions_supd =
    S.create Args.term_dimensions

  let elapsed_ns_s =
    Tick.e
    |> E.map (fun _ -> Clock.elapsed_ns ())
    |> S.hold 0L

  (*goto maybe put S on via mli*)
  module Input_event (* : S *) = struct

    module Listen = struct

      module Tcp = struct

        type event = [
          | `New_connection of Pier.t
          | `Closing_connection of Pier.t
          | `Error of (Pier.t * string)
          | `Recv_packet of Pier.t
          | `Sent_packet of Pier.t
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
          eupd @@ `Recv_packet {ip; port; conn_id}

        let sent_packet ~conn_id ~ip ~port ~header ~protocol =
          eupd @@ `Sent_packet {ip; port; conn_id}
        
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

        (* let e, eupd = E.create () *)

        let connecting ~conn_id:_ ~ip ~port =
          () (*goto*)

        let connected ~conn_id:_ ~ip ~port =
          () (*goto*)

        let writing ~conn_id:_ ~ip ~port ~data =
          () (*goto*)

        let error_connection ~conn_id:_ ~ip ~port ~err =
          () (*goto*)

        let error_writing ~conn_id:_ ~ip ~port ~err:_ ~msg =
          () (*goto*)

        let error_reading ~conn_id:_ ~ip ~port ~err =
          () (*goto*)

        let received_packet ~conn_id:_ ~ip ~port ~header ~protocol  = 
          () (*goto*)

        let sent_packet ~conn_id:_ ~ip ~port ~header ~protocol = 
          () (*goto*)

        let closing_flow ~conn_id:_ ~ip ~port =
          () (*goto*)

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

    let tcp_server_connections_e =
      let typ = `Server in
      let protocol = `Tcp in
      S.sample Tuple.mk2 
        Input_event.Listen.Tcp.e
        elapsed_ns_s
      |> E.fold (fun acc (event, elapsed_ns) -> match event with
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
        | `Recv_packet pier ->
          Conn_id_map.update pier.conn_id (function
            | None -> None (*shouldn't happen*)
            | Some conn ->
              let received_packets = succ conn.received_packets in
              Some { conn with received_packets }
          ) acc
        | `Sent_packet pier ->
          Conn_id_map.update pier.conn_id (function
            | None -> None (*shouldn't happen*)
            | Some conn ->
              let sent_packets = succ conn.sent_packets in
              Some { conn with sent_packets }
          ) acc
      ) Conn_id_map.empty

    let tcp_server_connections_s =
      S.hold Conn_id_map.empty tcp_server_connections_e

    let connections_s = tcp_server_connections_s
        
  end
  
  module Render = struct 

    open Notty 

    let render_conn elapsed_ns (_id, conn) =
      let sep_i = I.(string "|" <-> string "|") in
      let make_column title data_i =
        let title_i = I.string title in
        I.(sep_i <|> (title_i <-> data_i))
      in
      let protocol_i = make_column "prot" @@ match conn.protocol with
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
        make_column "uptime" @@ I.strf "%s" uptime_str
      in
      [
        [
          protocol_i;
          uptime_i;
          sent_packages_i;
          recv_packages_i;
        ];
      ]
      |> List.flatten
      |> I.hcat 
    
    let render_connections (this_name, (term_w, term_h), elapsed_ns, conns) =
      let conns = Conn_id_map.bindings conns in
      let client_conns, server_conns =
        List.partition (fun (_, conn) -> conn.typ = `Client) conns
      in
      let client_conn_images =
        client_conns |> List.map (render_conn elapsed_ns) 
      and server_conn_images =
        server_conns |> List.map (render_conn elapsed_ns)
      and name_i =
        this_name
        |> I.string 
        |> I.hsnap ~align:`Middle term_w in
      let client_conns_name_i =
        [
          I.string "Client-connections" |> I.hsnap ~align:`Middle term_w;
          I.string (String.make term_w '-');
        ]
        |> I.zcat
      in
      let server_conns_name_i =
        [
          I.string "Server-connections" |> I.hsnap ~align:`Middle term_w;
          I.string (String.make term_w '-');
        ]
        |> I.zcat
      in
      let make_section name_i = function
        | [] -> []
        | conn_is -> I.void 0 1 :: name_i :: conn_is 
      in
      [
        [name_i];
        make_section client_conns_name_i client_conn_images;
        make_section server_conns_name_i server_conn_images;
      ]
      |> List.flatten
      |> I.vcat 
    
    let image_s =
      S.l4 Tuple.mk4
        name_s
        term_dimensions_s
        elapsed_ns_s
        Data.connections_s
      |> S.map render_connections

    let image_e =
      image_s |> S.sample (fun _ image -> image) Tick.e

  end

  let image_e = Render.image_e

end
