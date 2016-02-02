open Bitstring
open Core.Std

type t = {
  source      : int;
  destination : int;
  seqnum      : int;
  acknum      : int;
  length      : int;
  checksum    : int;
}

val decode : Bitstring.t -> t option
val to_string : t -> string
