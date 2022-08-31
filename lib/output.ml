
module type S = sig

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
          m "new tcp connection from IP '%s' on port '%d'"
            (Ipaddr.to_string ip) port)

      let closing_connection ~ip ~port =
        Log.info (fun f -> f "closing tcp connection with %s:%d !"
            (Ipaddr.to_string ip) port
        )

      let error ~ip ~port ~err =
        Log.warn (fun f ->
          f "error reading data from established tcp connection with %s:%d:\n%s"
            (Ipaddr.to_string ip) port
            err)

      let data ~ip ~port ~data =
        Log.info (fun f ->
          f "read: %d bytes from %s:%d:\n%s"
            (Cstruct.length data)
            (Ipaddr.to_string ip) port
            (Cstruct.to_string data))

      let registered_listener ~port =
        Log.info (fun f -> f "registered tcp listener on port %d" port)

    end

    module Udp = struct

      let data ~ip ~port ~data =
        Log.info (fun f ->
          f "read: %d bytes from %s:%d:\n%s"
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
          m "connecting via tcp to %s on port %d"
            (Ipaddr.to_string ip) port)

      let connected ~ip ~port =
        Log.info (fun m ->
          m "connected via tcp to %s on port %d"
            (Ipaddr.to_string ip) port)

      let writing ~ip ~port ~data =
        Log.info (fun m ->
          m "writing via tcp to %s on port %d: %s"
            (Ipaddr.to_string ip) port data)

    end

    module Udp = struct

      let writing ~ip ~port ~data =
        Log.info (fun m ->
          m "writing via udp to %s on port %d: %s"
            (Ipaddr.to_string ip) port data)

    end

  end

end
