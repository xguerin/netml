type t = {
  source      : int;
  destination : int;
  length      : int;
  checksum    : int;
} [@@deriving yojson]

let decode data =
  match%bitstring data with
  | {|  source      : 16 : bigendian;
        destination : 16 : bigendian;
        length      : 16 : bigendian;
        checksum    : 16 : bigendian
    |} ->
    Some { source; destination; length; checksum }
  | {| _ |} -> None

let expand data =
  match%bitstring data with
  | {|  source      : 16 : bigendian;
        destination : 16 : bigendian;
        length      : 16 : bigendian;
        checksum    : 16 : bigendian;
        payload     : -1 : bitstring
    |} ->
    Some (payload)
  | {| _ |} -> None

