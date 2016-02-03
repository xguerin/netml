type t = {
  source      : int;
  destination : int;
  length      : int;
  checksum    : int;
} [@@deriving yojson]

val decode : Bitstring.t -> t option

val expand : Bitstring.t -> Bitstring.t option
