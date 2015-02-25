open Bitstring
open Core.Std

module Ethernet : sig

  module MAC : sig
    type t = int * int * int * int * int * int
  end

  module Protocol : sig
    type t =
      | Length of int
      | IPv4 of Bitstring.t
      | VLAN of Bitstring.t
      | Unsupported

    val of_int : int -> Bitstring.t -> t
  end

  type t = {
    source      : MAC.t;
    destination : MAC.t;
    protocol    : Protocol.t;
  } with fields

  val decode : (int * Bitstring.t) -> (int * t) option

end

module VLAN : sig

  type t = {
    pcp       : int;
    dei       : bool;
    vid       : int;
    protocol  : Ethernet.Protocol.t
  } with fields

  val decode : (int * Bitstring.t) -> (int * t) option

end

module IPv4 : sig

  module Address : sig
    type t = int * int * int * int
  end

  module Protocol : sig
    type t =
      | TCP of Bitstring.t
      | UDP of Bitstring.t
      | Unsupported

    val of_int : int -> Bitstring.t -> t
  end

  type t
  val decode : (int * Bitstring.t) -> (int * t) option
end
