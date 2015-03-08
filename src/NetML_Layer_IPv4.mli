open Bitstring
open Core.Std

module Address : sig
  type t = int * int * int * int
  val to_string : t -> string
end

module Protocol : sig
  type t =
    | TCP of NetML_Layer_TCP.t
    | UDP of NetML_Layer_UDP.t
    | Unsupported

  val decode : int -> Bitstring.t -> t
end

type t = {
  source      : Address.t;
  destination : Address.t;
  length      : int;
  protocol    : Protocol.t;
}

val decode : Bitstring.t -> t option
val to_string : t -> string
