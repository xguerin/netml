open Bitstring
open Core.Std

module Address : sig
  type t = int * int * int * int [@@deriving yojson]
end

module Header : sig
  type t = {
    source      : Address.t;
    destination : Address.t;
    length      : int;
    protocol    : NetML_Layer_IV.Protocol.t option;
  } [@@deriving yojson]
end

val decode : Bitstring.t -> (NetML_Layer_IV.Protocol.t * bitstring) option

val header : bitstring -> Header.t option
