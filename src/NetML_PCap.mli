module Endian : sig

  type t =
    | Big
    | Little
    | Native
    [@@deriving yojson]

  val to_bitstring_endian : t -> Bitstring.endian
  val of_bitstring_endian : Bitstring.endian -> t

end

module Precision : sig

   type t =
    | Microsecond
    | Nanosecond
  [@@deriving yojson]

  val to_string: t -> string

end

module Format : sig

  type t [@@deriving yojson]

  val decode : Int32.t -> t option
  val encode : t -> Int32.t

end

module Header : sig

  type t = {
    format  : Format.t;
    version : (int * int);
    snaplen : Int32.t;
    nettype : NetML_Layer_II.Protocol.t;
  } [@@deriving yojson]

end

module Packet : sig

  module Header : sig

    type t = {
      sec       : Int32.t;
      rsec      : Int32.t;
      incl_len  : Int32.t;
      orig_len  : Int32.t;
    } [@@deriving yojson]

  end

  type t = Header.t * Bitstring.t

  val timestamp : Precision.t -> t -> Int64.t

end

type t = Header.t * Bitstring.t

val open_file : string -> t option

val layerII : Header.t -> Packet.t -> NetML_Layer_II.t

val iter :  (Header.t -> Packet.t -> unit) -> t -> unit

val fold_left : ('a -> Header.t -> Packet.t -> 'a) -> 'a -> t -> 'a
