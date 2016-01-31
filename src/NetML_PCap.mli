open Bitstring

exception Bad_format of int32

(** PCap endianness type *)
module Endian : sig
  type t =
    | Big
    | Little
    | Native
    [@@deriving yojson]
  val to_bitstring_endian : t -> Bitstring.endian
  val of_bitstring_endian : Bitstring.endian -> t
end

(** PCap format definition *)
module Format : sig
  type t [@@deriving yojson]
  val of_int : int32 -> t
  val to_int : Endian.t -> t -> int32
  val to_endian : int32 -> Endian.t
  val to_bitstring_endian : int32 -> Bitstring.endian
  val to_string : t -> string
end

(** PCap global and packet header definitions *)
module Header : sig
  type t = {
    endian  : Endian.t;
    format  : Format.t;
    version : (int * int);
    snaplen : Int32.t;
  } [@@deriving yojson]
end

(** PCap packet definition *)
module Packet : sig
  module Header : sig
    type t = {
      sec       : Int32.t;
      rsec      : Int32.t;
      incl_len  : Int32.t;
      orig_len  : Int32.t;
    } [@@deriving yojson]
  end
  type 'a t
  type usec_format
  type nsec_format
  val create_usec : Header.t -> bitstring -> usec_format t
  val create_nsec : Header.t -> bitstring -> nsec_format t
  val header : 'a t -> Header.t
  val timestamp_ns : usec_format t -> Int64.t
  val timestamp_ns : nsec_format t -> Int64.t
end

(** PCap file type *)
type t

(** Open a PCAP file *)
val open_file : string -> t option

(** Retrieve the global header of a PCAP file *)
val header : t -> Header.t

(** Iterate over a PCAP file *)
val iter :  ('a Packet.t -> unit) -> t -> unit
