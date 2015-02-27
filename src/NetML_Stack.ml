open Bitstring

module IPv4 = struct

  module Address = struct
    type t = int * int * int * int

    let to_string v =
      let (a, b, c, d) = v in
      Printf.sprintf "%03d.%03d.%03d.%03d" a b c d
  end

  module Protocol = struct
    type t =
      | TCP of Bitstring.t
      | UDP of Bitstring.t
      | Unsupported

    let of_int content_type bs =
      match content_type with
      | 0x6   -> TCP bs
      | 0x11  -> UDP bs
      | _     -> Unsupported
  end

  type t = {
    source      : Address.t;
    destination : Address.t;
    length      : int;
    protocol    : Protocol.t;
  } with fields

  let decode data =
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
        payload     : (length - ihl * 4) : bitstring
    } ->
      let source = (s0, s1, s2, s3) in
      let destination = (d0, d1, d2, d3) in
      let protocol = Protocol.of_int proto payload in
      Some { source; destination; length; protocol }
  | { _ } -> None

  let to_string v =
    Printf.sprintf "IPv4 (%s -> %s)" (Address.to_string v.source) (Address.to_string v.destination)

end

module Ethernet = struct

  module MAC = struct
    type t = int * int * int * int * int * int

    let to_string v =
      let (s0, s1, s2, s3, s4, s5) = v in
      Printf.sprintf "%02X:%02X:%02X:%02X:%02X:%02X" s0 s1 s2 s3 s4 s5
  end

  module VLAN = struct
    type t = {
      pcp       : int;
      dei       : bool;
      vid       : int;
    } with fields
  end

  module Protocol = struct

    type t =
      | Length of int
      | IPv4 of IPv4.t
      | VLAN of VLAN.t * t
      | Unsupported

    let rec decode payload =
      bitmatch payload with
      | { ( 0x8100 | 0x0081 ) : 16;
          pcp                 : 3;
          dei                 : 1;
          vid                 : 12 : bigendian;
          payload             : -1 : bitstring
        } ->
          let nv = { VLAN.pcp; VLAN.dei; VLAN.vid } in
          begin match decode payload with
          | Some v  -> Some (VLAN (nv, v))
          | None    -> None
          end
      | { ( 0x0800 | 0x0008 ) : 16;
          payload             : -1 : bitstring
        } ->
          begin match IPv4.decode payload with
          | Some v  -> Some (IPv4 v)
          | None    -> None
          end
      | { _ } -> Some Unsupported

  end

  type t = {
    destination : MAC.t;
    source      : MAC.t;
    protocol    : Protocol.t;
  } with fields

  let decode data =
    bitmatch data with
    | { d0 : 8; d1 : 8; d2 : 8; d3 : 8; d4 : 8; d5 : 8;
        s0 : 8; s1 : 8; s2 : 8; s3 : 8; s4 : 8; s5 : 8;
        payload     : -1 : bitstring
      } ->
        let destination = (d0, d1, d2, d3, d4, d5) in
        let source = (s0, s1, s2, s3, s4, s5) in
        begin match Protocol.decode payload with
        | Some protocol -> Some { destination; source; protocol }
        | None          -> None
        end
    | { _ } -> None

  let to_string v =
    Printf.sprintf "ETH (%s -> %s)" (MAC.to_string v.source) (MAC.to_string v.destination)

end

