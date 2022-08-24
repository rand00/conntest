
module type S = sig

  module Listen : sig

    module Tcp : sig
      val new_connection : ip:Ipaddr.t -> port:int -> unit
      val closing_connection : ip:Ipaddr.t -> port:int -> unit
      val error : ip:Ipaddr.t -> port:int -> err:string -> unit
    end

  end

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
        Log.info (fun f -> f "Closing connection!")

      let error ~ip ~port ~err =
        Log.warn (fun f ->
          f "Error reading data from established connection: %s" err)

    end

  end

  module Connect = struct

    module Tcp = struct


    end

  end

end
