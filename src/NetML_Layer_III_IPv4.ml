open Bitstring

module Address = struct
  type t = int * int * int * int [@@deriving yojson]
end

module Header = struct
  type t = {
    source      : Address.t;
    destination : Address.t;
    length      : int;
    protocol    : NetML_Layer_IV.Protocol.t option;
  } [@@deriving yojson]
end

let protocol_of_int = function
  | 0x06 -> Some NetML_Layer_IV.Protocol.TCP
  | 0x11 -> Some NetML_Layer_IV.Protocol.UDP
  | _ -> None

let decode data =
  match%bitstring data with
  | {|  4           : 4;
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
        payload     : (length - ihl * 4) * 8 : bitstring
    |} ->
    begin match protocol_of_int proto with
      | Some (p) -> Some (p, payload)
      | None -> None
    end
  | {| _ |} -> None

let header data =
  match%bitstring data with
  | {|  4           : 4;
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
        d0 : 8; d1 : 8; d2 : 8; d3 : 8
    |} ->
    let open Header in
    let source = (s0, s1, s2, s3) in
    let destination = (d0, d1, d2, d3) in
    Some { source; destination; length; protocol = protocol_of_int proto }
  | {| _ |} -> None
