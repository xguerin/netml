module Address : sig
  type t = int * int * int * int [@@deriving yojson]
end

type t = {
  source      : Address.t;
  destination : Address.t;
  length      : int;
  protocol    : NetML_Layer_IV.Protocol.t option;
} [@@deriving yojson]

val decode : Bitstring.t -> t option

val expand : Bitstring.t -> (NetML_Layer_IV.Protocol.t * Bitstring.t) option

