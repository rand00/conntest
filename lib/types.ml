module Conn_id = struct

  module T = struct

    type t = string

  end
  include T

  let compare = String.compare

end

type conn_id = Conn_id.t

type protocol = [ `Tcp | `Udp ]

module Pier = struct 

  type t = {
    protocol : protocol;
    ip : Ipaddr.t;
    port : int;
    conn_id : Conn_id.t;
    (*< Note: this is the local id
      - the conn-id sent via header is for sending back*)
  }

end

type pier = Pier.t
