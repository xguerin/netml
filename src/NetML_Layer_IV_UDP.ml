open Bitstring
open Core.Std

type t = {
  source      : int;
  destination : int;
  length      : int;
  checksum    : int;
}

let decode data =
  match%bitstring data with
  | {|  source      : 16 : bigendian;
        destination : 16 : bigendian;
        length      : 16 : bigendian;
        checksum    : 16 : bigendian
    |} ->
    Some { source; destination; length; checksum }
  | {| _ |} -> None

let to_string v =
  Printf.sprintf "UDP:(%5d -> %5d, %5d)" v.source v.destination v.length

