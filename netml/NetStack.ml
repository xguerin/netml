open Bitstring

module Ethernet = struct

  module MAC = struct
    type t = int * int * int * int * int * int

    let to_string v =
      let (s0, s1, s2, s3, s4, s5) = v in
      Printf.sprintf "%02X:%02X:%02X:%02X:%02X:%02X" s0 s1 s2 s3 s4 s5
  end

  module Protocol = struct
    type t =
      | Length of int
      | IPv4 of int * Bitstring.t
      | VLAN of int * Bitstring.t
      | Unsupported

    let of_int len_or_type (len, bs) =
      if len_or_type < 1536 then Length len_or_type
      else if len_or_type = 0x0800 then IPv4 (len, bs)
      else if len_or_type = 0x8100 then VLAN (len, bs)
      else Unsupported
  end

  type t = {
    destination : MAC.t;
    source      : MAC.t;
    protocol    : Protocol.t;
  } with fields

  let decode (len, data) =
    bitmatch data with
    | { d0 : 8; d1 : 8; d2 : 8; d3 : 8; d4 : 8; d5 : 8;
        s0 : 8; s1 : 8; s2 : 8; s3 : 8; s4 : 8; s5 : 8;
        len_or_type : 16 : bigendian;
        payload     : -1 : bitstring
      } ->
        let destination = (d0, d1, d2, d3, d4, d5) in
        let source = (s0, s1, s2, s3, s4, s5) in
        let protocol = Protocol.of_int len_or_type (len - 14, payload) in
        Some { destination; source; protocol }
    | { _ } -> None

  let to_string v =
    Printf.sprintf "ETH (%s -> %s)" (MAC.to_string v.source) (MAC.to_string v.destination)
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
        let protocol = Ethernet.Protocol.of_int len_or_type (len - 4, payload) in
        Some { pcp; dei; vid; protocol }
    | { _ } -> None

end

module IPv4 = struct

  module Address = struct
    type t = int * int * int * int

    let to_string v =
      let (a, b, c, d) = v in
      Printf.sprintf "%d.%d.%d.%d" a b c d
  end

  module Protocol = struct
    type t =
      | TCP of int * Bitstring.t
      | UDP of int * Bitstring.t
      | Unsupported

    let of_int content_type (len, bs) =
      match content_type with
      | 0x6   -> TCP (len, bs)
      | 0x11  -> UDP (len, bs)
      | _     -> Unsupported
  end

  type t = {
    source      : Address.t;
    destination : Address.t;
    length      : int;
    protocol    : Protocol.t;
  } with fields

  let decode (len, data) =
    bitmatch data with
    | { 4           : 4;
        ihl         : 4;
        _           : 6; (* dscp *)
        _           : 2; (* ecn *)
        length      : 16  : bigendian;
        _           : 16  : bigendian; (* ident *)
        _           : 3; (* flags *)
        _           : 13  : bigendian; (* fragment offset *)
        _           : 8; (* ttl *)
        proto       : 8;
        _           : 16  : bigendian; (* checksum *)
        s0 : 8; s1 : 8; s2 : 8; s3 : 8;
        d0 : 8; d1 : 8; d2 : 8; d3 : 8;
        payload     : len : bitstring
    } ->
      let source = (s0, s1, s2, s3) in
      let destination = (d0, d1, d2, d3) in
      let protocol = Protocol.of_int proto (len - ihl * 4, payload) in
      Some { source; destination; length; protocol }
  | { _ } -> None

  let to_string v =
    Printf.sprintf "IPv4 (%s -> %s)" (Address.to_string v.source) (Address.to_string v.destination)

end
