open Bitstring
open Core.Std

module Ethernet : sig

  module MAC : sig
    type t = int * int * int * int * int * int
    val to_string : t -> string
  end

  module Protocol : sig
    type t =
      | Length of int
      | IPv4 of int * Bitstring.t
      | VLAN of int * Bitstring.t
      | Unsupported

    val of_int : int -> int * Bitstring.t -> t
  end

  type t = {
    destination : MAC.t;
    source      : MAC.t;
    protocol    : Protocol.t;
  } with fields

  val decode : (int * Bitstring.t) -> t option
  val to_string : t -> string

end

module VLAN : sig

  type t = {
    pcp       : int;
    dei       : bool;
    vid       : int;
    protocol  : Ethernet.Protocol.t
  } with fields

  val decode : (int * Bitstring.t) -> t option

end

module IPv4 : sig

  module Address : sig
    type t = int * int * int * int
    val to_string : t -> string
  end

  module Protocol : sig
    type t =
      | TCP of int * Bitstring.t
      | UDP of int * Bitstring.t
      | Unsupported

    val of_int : int -> int * Bitstring.t -> t
  end

  type t = {
    source      : Address.t;
    destination : Address.t;
    length      : int;
    protocol    : Protocol.t;
  } with fields

  val decode : (int * Bitstring.t) -> t option
  val to_string : t -> string

end
