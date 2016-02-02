open Bitstring
open NetML_Layer_III

module MAC = struct
  type t = int * int * int * int * int * int [@@deriving yojson]
end

module VLAN = struct
  type t = {
    pcp : int;
    dei : bool;
    vid : int;
  } [@@deriving yojson]
end

module Header = struct
  type t = {
    destination : MAC.t;
    source      : MAC.t;
    protocol    : NetML_Layer_III.Protocol.t option;
  } [@@deriving yojson]
end

type t = (Header.t * VLAN.t list * Bitstring.bitstring)

let rec decode_protocol vlans payload =
  let open NetML.Layer.III in
  match%bitstring payload with
  | {|  ( 0x8100 | 0x0081 ) : 16;
        pcp                 : 3;
        dei                 : 1;
        vid                 : 12 : bigendian;
        next                : -1 : bitstring
    |} ->
    let open VLAN in
    let vlan = { pcp = pcp; dei = dei; vid = vid } in
    decode_protocol (vlans @ [ vlan ]) next
  | {|  ( 0x0800 | 0x0008 ) : 16;
        next                : -1 : bitstring
    |} -> (vlans, Some Protocol.IPv4, next)
  | {| _ |} -> ([], None, payload)

let decode data =
  match%bitstring data with
  | {|  _ : 8; _ : 8; _ : 8; _ : 8; _ : 8; _ : 8;
        _ : 8; _ : 8; _ : 8; _ : 8; _ : 8; _ : 8;
        payload : -1 : bitstring
    |} ->
    let open Header in
    let (_, proto, rem) = decode_protocol [] payload in
    begin match proto with
      | Some (p) -> Some (p, rem)
      | None -> None
    end
  | {| _ |} -> None
