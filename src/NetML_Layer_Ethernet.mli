open Bitstring
open Core.Std
open NetML_Layer_IPv4

module MAC : sig
  type t = int * int * int * int * int * int
  val to_string : t -> string
end

module VLAN : sig
  type t = {
    pcp : int;
    dei : int;
    vid : int;
  }
end

module Protocol : sig
  type t =
    | Length of int
    | IPv4 of NetML_Layer_IPv4.t
    | VLAN of VLAN.t * t
    | Unsupported

  val decode : Bitstring.t -> t option
end

type t = {
  destination : MAC.t;
  source      : MAC.t;
  protocol    : Protocol.t;
}

val decode : Bitstring.t -> t option
val to_string : t -> string
