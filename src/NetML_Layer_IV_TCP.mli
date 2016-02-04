type t = {
  source      : int;
  destination : int;
  seqnum      : Int32.t;
  acknum      : Int32.t;
  checksum    : int;
} [@@deriving yojson]

val decode : Bitstring.t -> t option

val expand : Bitstring.t -> Bitstring.t option
