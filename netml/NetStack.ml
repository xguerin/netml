open Bitstring

module Ethernet = struct

  module MAC = struct
    type t = int * int * int * int * int * int
  end

  module Protocol = struct
    type t =
      | Length of int
      | IPv4 of Bitstring.t
      | VLAN of Bitstring.t
      | Unsupported

    let of_int v b =
      if v < 1536 then Length v
      else if v = 0x0800 then IPv4 (b)
      else if v = 0x8100 then VLAN (b)
      else Unsupported
  end

  type t = {
    source      : MAC.t;
    destination : MAC.t;
    protocol    : Protocol.t;
  } with fields

  let decode (len, data) =
    bitmatch data with
    | { s0 : 8; s1 : 8; s2 : 8; s3 : 8; s4 : 8; s5 : 8;
        d0 : 8; d1 : 8; d2 : 8; d3 : 8; d4 : 8; d5 : 8;
        len_or_type : 16 : bigendian;
        payload     : -1 : bitstring
      } ->
        let source = (s0, s1, s2, s3, s4, s5) in
        let destination = (d0, d1, d2, d3, d4, d5) in
        let protocol = Protocol.of_int len_or_type payload in
        Some (len - 14, { source; destination; protocol })
    | { _ } -> None
end

module VLAN = struct

  type t = {
    pcp       : int;
    dei       : bool;
    vid       : int;
    protocol  : Ethernet.Protocol.t
  } with fields

  let decode (len, data) =
    bitmatch data with
    | { pcp               : 3;
        dei               : 1;
        vid               : 12 : bigendian;
        len_or_type       : 16 : bigendian;
        payload           : -1 : bitstring
      } ->
        let protocol = Ethernet.Protocol.of_int len_or_type payload in
        Some (len - 4, { pcp; dei; vid; protocol })
    | { _ } -> None

end

module IPv4 = struct

  module Address = struct
    type t = int * int * int * int
  end

  module Protocol = struct
    type t =
      | TCP of Bitstring.t
      | UDP of Bitstring.t
      | Unsupported

    let of_int v b =
      if v = 0x6 then TCP (b)
      else if v = 0x11 then UDP (b)
      else Unsupported
  end

  type t = {
    version     : int;
    source      : Address.t;
    destination : Address.t;
    length      : int;
    protocol    : Protocol.t;
  } with fields

  let decode (len, data) =
    bitmatch data with
    | { ihl         : 4;
        version     : 4;
        _           : 2; (* ecn *)
        _           : 6; (* dscp *)
        length      : 16  : bigendian;
        _           : 16  : bigendian; (* ident *)
        _           : 13  : bigendian; (* fragment offset *)
        _           : 3; (* flags *)
        _           : 8; (* ttl *)
        proto       : 8;
        _           : 16  : bigendian; (* checksum *)
        s0 : 8; s1 : 8; s2 : 8; s3 : 8;
        d0 : 8; d1 : 8; d2 : 8; d3 : 8;
        payload     : len : bitstring
    } ->
      let source = (s0, s1, s2, s3) in
      let destination = (d0, d1, d2, d3) in
      let ip = { version; source; destination; length; protocol = Protocol.of_int proto payload } in
      Some (len - ihl * 4, ip)
  | { _ } -> None

end
