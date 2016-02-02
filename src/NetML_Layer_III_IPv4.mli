open Bitstring
open Core.Std

module Address : sig
  type t = int * int * int * int
end

module Header : sig
  type t = {
    source      : Address.t;
    destination : Address.t;
    length      : int;
    protocol    : NetML_Layer_IV.Protocol.t;
  }
end

val decode : Bitstring.t -> (NetML_Layer_IV.Protocol.t * bitstring) option
