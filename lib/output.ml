
(*goto rename this module to Ui ? - Output can mean many things.. *)

module type S = sig

  (*> goto all errors should be explicit, so user can get all the info wanted
    .. and notty interface needs error values instead of just a string 
  *)
  
  module Listen : sig

    module Tcp : sig
      val new_connection : ip:Ipaddr.t -> port:int -> unit
      val closing_connection : ip:Ipaddr.t -> port:int -> unit
      val error : ip:Ipaddr.t -> port:int -> err:string -> unit
      val data : ip:Ipaddr.t -> port:int -> data:Cstruct.t -> unit
      val registered_listener : port:int -> unit
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
      val writing : ip:Ipaddr.t -> port:int -> data:string -> unit
      val error_connection : ip:Ipaddr.t -> port:int -> err:string -> unit
      val error_writing : ip:Ipaddr.t -> port:int -> err:Tcpip.Tcp.write_error
        -> unit
      val error_writing_str : ip:Ipaddr.t -> port:int -> err:string -> unit
      val wrote_data : ip:Ipaddr.t -> port:int -> unit
      val closing_flow : ip:Ipaddr.t -> port:int -> unit
      val closed_flow : ip:Ipaddr.t -> port:int -> unit
          
    end

    module Udp : sig

      val writing : ip:Ipaddr.t -> port:int -> data:string -> unit

    end
    
  end

end

module Log_stdout : S = struct

  let src = Logs.Src.create "conntest" ~doc:"conntest events"
  module Log = (val Logs.src_log src : Logs.LOG)
  
  module Listen = struct

    module Tcp = struct
      
      let new_connection ~ip ~port =
        Log.info (fun m ->
          m "new tcp connection from %s:%d"
            (Ipaddr.to_string ip) port)

      let closing_connection ~ip ~port =
        Log.info (fun f -> f "closing tcp connection to %s:%d"
            (Ipaddr.to_string ip) port
        )

      let error ~ip ~port ~err =
        Log.warn (fun f ->
          f "error reading data from tcp connection %s:%d:\n%s"
            (Ipaddr.to_string ip) port
            err)

      let data ~ip ~port ~data =
        Log.info (fun f ->
          f "read %d bytes from %s:%d:\n%s"
            (Cstruct.length data)
            (Ipaddr.to_string ip) port
            (Cstruct.to_string data))

      let registered_listener ~port =
        Log.info (fun f -> f "registered tcp listener on port %d" port)

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
            (Ipaddr.to_string ip) port data)

      let error_connection ~ip ~port ~err =
        Log.warn (fun f ->
          f "error connecting via tcp to %s:%d:\n%s"
            (Ipaddr.to_string ip) port
            err)

      let error_writing ~ip ~port ~err =
        let err = Fmt.str "%a" Tcpip.Tcp.pp_write_error err in
        Log.warn (fun f ->
          f "error writing via tcp to %s:%d:\n%s"
            (Ipaddr.to_string ip) port
            err)

      let error_writing_str ~ip ~port ~err =
        Log.warn (fun f ->
          f "error writing via tcp to %s:%d:\n%s"
            (Ipaddr.to_string ip) port
            err)
        
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
