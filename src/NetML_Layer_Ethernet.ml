open Bitstring
open NetML_Layer_IPv4

module MAC = struct
  type t = int * int * int * int * int * int

  let to_string v =
    let (s0, s1, s2, s3, s4, s5) = v in
    Printf.sprintf "%02X:%02X:%02X:%02X:%02X:%02X" s0 s1 s2 s3 s4 s5
end

module VLAN = struct
  type t = {
    pcp       : int;
    dei       : int;
    vid       : int;
  }
end

module Protocol = struct

  type t =
    | Length of int
    | IPv4 of NetML_Layer_IPv4.t
    | VLAN of VLAN.t * t
    | Unsupported

  let rec decode payload =
    match%bitstring payload with
    | {|  ( 0x8100 | 0x0081 ) : 16;
          pcp                 : 3;
          dei                 : 1;
          vid                 : 12 : bigendian;
          payload             : -1 : bitstring
      |} ->
        let nv = { VLAN.pcp; VLAN.dei; VLAN.vid } in
        begin match decode payload with
        | Some v  -> Some (VLAN (nv, v))
        | None    -> None
        end
    | {|  ( 0x0800 | 0x0008 ) : 16;
          payload             : -1 : bitstring
      |} ->
        begin match NetML_Layer_IPv4.decode payload with
        | Some v  -> Some (IPv4 v)
        | None    -> None
        end
    | {| _ |} -> Some Unsupported

end

type t = {
  destination : MAC.t;
  source      : MAC.t;
  protocol    : Protocol.t;
}

let decode data =
  match%bitstring data with
  | {|  d0 : 8; d1 : 8; d2 : 8; d3 : 8; d4 : 8; d5 : 8;
        s0 : 8; s1 : 8; s2 : 8; s3 : 8; s4 : 8; s5 : 8;
        payload : -1 : bitstring
    |} ->
      let destination = (d0, d1, d2, d3, d4, d5) in
      let source = (s0, s1, s2, s3, s4, s5) in
      begin match Protocol.decode payload with
      | Some protocol -> Some { destination; source; protocol }
      | None          -> None
      end
  | {| _ |} -> None

let to_string v =
  Printf.sprintf "ETH:(%s -> %s)" (MAC.to_string v.source) (MAC.to_string v.destination)
