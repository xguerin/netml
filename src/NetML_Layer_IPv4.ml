open Bitstring

module Address = struct
  type t = int * int * int * int

  let to_string v =
    let (a, b, c, d) = v in
    Printf.sprintf "%03d.%03d.%03d.%03d" a b c d
end

module Protocol = struct
  type t =
    | TCP of Bitstring.t
    | UDP of NetML_Layer_UDP.t
    | Unsupported

  let decode proto bs =
    match proto with
    | 0x6   -> TCP bs
    | 0x11  ->
        let udp = NetML_Layer_UDP.decode bs in
        begin match udp with
        | Some v  -> UDP v
        | None    -> Unsupported
        end
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
    let protocol = Protocol.decode proto payload in
    Some { source; destination; length; protocol }
| { _ } -> None

let to_string v =
  Printf.sprintf "IPv4 (%s -> %s)" (Address.to_string v.source) (Address.to_string v.destination)
