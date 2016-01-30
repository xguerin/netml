open Bitstring

module Format : sig
  type t
  val of_int : int32 -> t
  val to_int : Bitstring.endian -> t -> int32
  val to_endian : int32 -> Bitstring.endian
  val to_string : t -> string
end  

type t

val open_file : string -> t option
val iter : f:(ts:int -> data:Bitstring.bitstring -> len:int -> unit) -> t -> unit

exception Bad_format of int32
