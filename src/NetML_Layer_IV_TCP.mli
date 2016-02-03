type t = {
  source      : int;
  destination : int;
  seqnum      : int;
  acknum      : int;
  checksum    : int;
} [@@deriving yojson]

val decode : Bitstring.t -> t option

val expand : Bitstring.t -> Bitstring.t option
