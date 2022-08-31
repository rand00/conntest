
module type S = sig

  module Listen : sig

    module Tcp : sig
      val new_connection : ip:Ipaddr.t -> port:int -> unit
      val closing_connection : ip:Ipaddr.t -> port:int -> unit
      val error : ip:Ipaddr.t -> port:int -> err:string -> unit
      val data : ip:Ipaddr.t -> port:int -> data:Cstruct.t -> unit
      val registered_listener : port:int -> unit
    end

  end

  (*goto*)
  module Connect : sig

    module Tcp : sig
      
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
        Log.info (fun f -> f "closing connection!")

      let error ~ip ~port ~err =
        Log.warn (fun f ->
          f "error reading data from established connection: %s" err)

      let data ~ip ~port ~data =
        Log.info (fun f ->
          f "read: %d bytes:\n%s"
            (Cstruct.length data)
            (Cstruct.to_string data))

      let registered_listener ~port =
        Log.info (fun f -> f "registered tcp listener on port %d" port)

    end

  end

  module Connect = struct

    module Tcp = struct


    end

  end

end
