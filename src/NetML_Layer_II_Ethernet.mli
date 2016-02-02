open Bitstring
open Core.Std
open NetML_Layer_III

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

module Header : sig
  type t = {
    destination : MAC.t;
    source      : MAC.t;
    protocol    : NetML_Layer_III.Protocol.t option;
  } [@@deriving yojson]
end

type t = (Header.t * VLAN.t list * Bitstring.bitstring)

val decode : Bitstring.t -> (NetML_Layer_III.Protocol.t * bitstring) option
