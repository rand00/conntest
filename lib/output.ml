
(*goto rename this module to Ui ? - Output can mean many things.. *)

type connect_tcp_read_error = [
  | `Eof
  | `Msg of string
  | `Read of (Tcpip.Tcp.error option * string (*message*))
  (*< Note: the option is because of private variants*)
]

module type S = sig

  val set_name : string -> unit
  val set_term_dimensions : (int * int) -> unit
  
  module Listen : sig

    module Tcp : sig
      val new_connection : conn_id:string -> ip:Ipaddr.t -> port:int -> unit
      val closing_connection : conn_id:string -> ip:Ipaddr.t -> port:int -> unit
      (*> goto all errors should be explicit, so user can get all the info wanted
        .. and notty interface needs error values instead of just a string 
      *)
      val error : conn_id:string  -> ip:Ipaddr.t -> port:int -> err:string -> unit
      val registered_listener : port:int -> unit
      val packet : ip:Ipaddr.t -> port:int -> Packet.t -> unit
    end

    module Udp : sig
      val data : ip:Ipaddr.t -> port:int -> data:Cstruct.t -> unit
      val registered_listener : port:int -> unit
    end
    
  end

  module Connect : sig

    module Tcp : sig

      val connecting : ip:Ipaddr.t -> port:int -> unit
      val connected : ip:Ipaddr.t -> port:int -> unit
      val writing : ip:Ipaddr.t -> port:int -> data:Cstruct.t -> unit
      (*> goto all errors should be explicit, so user can get all the info wanted
        .. and notty interface needs error values instead of just a string 
      *)
      val error_connection : ip:Ipaddr.t -> port:int -> err:string -> unit
      val error_writing : ip:Ipaddr.t -> port:int
        -> err:(Tcpip.Tcp.write_error option) -> msg:string
        -> unit
      val error_reading : ip:Ipaddr.t -> port:int
        -> err:connect_tcp_read_error 
        -> unit
      val wrote_data : ip:Ipaddr.t -> port:int -> unit
      val closing_flow : ip:Ipaddr.t -> port:int -> unit
      val closed_flow : ip:Ipaddr.t -> port:int -> unit
          
    end

    module Udp : sig

      val writing : ip:Ipaddr.t -> port:int -> data:string -> unit

    end
    
  end

end

module Log_stdout () : S = struct

  let set_name _ = ()
  let set_term_dimensions _ = ()
  
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

      let packet ~ip ~port packet =
        let open Packet.T in
        Log.info (fun f ->
          f "got packet from %s:%d:\n---- header:\n%s\n---- data:\n%s"
            (Ipaddr.to_string ip) port
            (packet.header |> Packet.Header.to_string)
            (packet.data |> preview_big_string)
            (*< goto depend explicitly on this or do something else*)
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

      let connecting ~ip ~port =
        Log.info (fun m ->
          m "connecting via tcp to %s:%d"
            (Ipaddr.to_string ip) port)

      let connected ~ip ~port =
        Log.info (fun m ->
          m "connected via tcp to %s:%d"
            (Ipaddr.to_string ip) port)

      let writing ~ip ~port ~data =
        Log.info (fun m ->
          m "writing via tcp to %s:%d: %s"
            (Ipaddr.to_string ip) port
            (data |> Cstruct.to_string |> preview_big_string)
        ) 

      let error_connection ~ip ~port ~err =
        Log.warn (fun f ->
          f "error connecting via tcp to %s:%d:\n%s"
            (Ipaddr.to_string ip) port
            err)

      let error_writing ~ip ~port ~err:_ ~msg =
        Log.warn (fun f ->
          f "error writing via tcp to %s:%d:\n%s"
            (Ipaddr.to_string ip) port msg
        )

      let error_reading ~ip ~port ~err =
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
        
      let wrote_data ~ip ~port = 
        Log.info (fun m ->
          m "wrote via tcp to %s:%d" (Ipaddr.to_string ip) port)
        
      let closing_flow ~ip ~port =
        Log.info (fun m ->
          m "closing tcp flow to %s:%d" (Ipaddr.to_string ip) port)
        
      let closed_flow ~ip ~port =
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

(*> goto make this fit S signature, or inner module?*)
module Notty_ui (Time : Mirage_time.S) = struct

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
        start_time : unit; (*goto find out which time format makes sense with mirage*)
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

  (*goto maybe put S on via mli*)
  module Input_event (* : S *) = struct

    let (name_s : string option S.t), name_supd = S.create None
    let set_name s = name_supd @@ Some s

    let (term_dimensions_s : (int * int) S.t), term_dimensions_supd =
      S.create (70, 20)
    let set_term_dimensions v = term_dimensions_supd
    
    module Listen = struct

      module Tcp = struct

        type event = [
          | `New_connection of Pier.t
          | `Closing_connection of Pier.t
          | `Error of (Pier.t * string)
          | `Packet of Pier.t
        ]
        
        let (e : event E.t), eupd = E.create ()

        (*goto this could get a temporary id passed
          .. so id either is `Temp _ | `Clients _
        *)
        let new_connection ~conn_id ~ip ~port =
          eupd @@ `New_connection {ip; port; conn_id}

        let closing_connection ~conn_id ~ip ~port =
          eupd @@ `Closing_connection {ip; port; conn_id}

        let error ~conn_id ~ip ~port ~err =
          eupd @@ `Error ({ip; port; conn_id}, (err : string))

        let registered_listener ~port = () 

        (*> Note: there is also a remote conn-id in header*)
        let packet ~conn_id ~ip ~port packet =
          eupd @@ `Packet {ip; port; conn_id}

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

        let connecting ~ip ~port =
          () (*goto*)

        let connected ~ip ~port =
          () (*goto*)

        let writing ~ip ~port ~data =
          () (*goto*)

        let error_connection ~ip ~port ~err =
          () (*goto*)

        let error_writing ~ip ~port ~err:_ ~msg =
          () (*goto*)

        let error_reading ~ip ~port ~err =
          () (*goto*)

        let wrote_data ~ip ~port = 
          () (*goto*)

        let closing_flow ~ip ~port =
          () (*goto*)

        let closed_flow ~ip ~port =
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
      Input_event.Listen.Tcp.e
      |> E.fold (fun acc -> function
        | `New_connection pier ->
          let start_time = () in (*goto pass this via event*)
          let conn = Connection.make ~typ ~protocol ~start_time ~pier in
          acc |> Conn_id_map.add pier.conn_id conn
        | `Closing_connection pier ->
          Conn_id_map.remove pier.conn_id acc
        | `Error (pier, _err) ->
          Conn_id_map.update pier.conn_id (function
            | None -> None
            | Some conn -> Some { conn with error = true }
          ) acc
        | `Packet pier ->
          Conn_id_map.update pier.conn_id (function
            | None -> None (*shouldn't happen*)
            | Some conn ->
              let received_packets = succ conn.received_packets in
              Some { conn with received_packets }
          ) acc
      ) Conn_id_map.empty

    let tcp_server_connections_s =
      S.hold Conn_id_map.empty tcp_server_connections_e

    let connections_s = tcp_server_connections_s
        
  end
  
  module Render = struct 

    open Notty 

    let render_connections (this_name, (term_w, term_h), conns) =
      let conns = Conn_id_map.bindings conns in
      let client_conns, server_conns =
        List.partition (fun (_, conn) -> conn.typ = `Client) conns
      in
      let client_conn_is = failwith "todo" in
      let server_conn_is = failwith "todo" in
      let name_i =
        this_name
        |> Option.value ~default:"N/A"
        |> I.string 
        |> I.hsnap ~align:`Middle term_w in
      let client_conns_name_i =
        [
          I.string (String.make term_w '-');
          I.string "Client-connections" |> I.hsnap ~align:`Middle term_w;
        ]
        |> I.zcat
      in
      let server_conns_name_i =
        [
          I.string (String.make term_w '-');
          I.string "Server-connections" |> I.hsnap ~align:`Middle term_w;
        ]
        |> I.zcat
      in
      [
        [name_i];
        [client_conns_name_i];
        client_conn_is;
        [server_conns_name_i];
        server_conn_is;
      ]
      |> List.flatten
      |> I.vcat 
    
    let image_s =
      S.l3 Tuple.mk3
        Input_event.name_s
        Input_event.term_dimensions_s
        Data.connections_s
      |> S.map render_connections

    let image_e =
      image_s |> S.sample (fun _ image -> image) Tick.e

  end

  let image_e = Render.image_e

end
