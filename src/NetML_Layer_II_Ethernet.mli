module MAC : sig
  type t = int * int * int * int * int * int [@@deriving yojson]
end

module VLAN : sig
  type t = {
    pcp : int;
    dei : bool;
    vid : int;
  } [@@deriving yojson]
end

type t = {
  destination : MAC.t;
  source      : MAC.t;
  vlans       : VLAN.t list;
  protocol    : NetML_Layer_III.Protocol.t option;
} [@@deriving yojson]

val decode : Bitstring.t -> t option

val expand : Bitstring.t -> (NetML_Layer_III.Protocol.t * Bitstring.t) option
