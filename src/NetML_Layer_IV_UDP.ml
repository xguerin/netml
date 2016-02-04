type t = {
  source      : int;
  destination : int;
  length      : int;
  checksum    : int;
} [@@deriving yojson]

let decode_header data =
  match%bitstring data with
  | {|  source      : 16                : bigendian;
        destination : 16                : bigendian;
        length      : 16                : bigendian;
        checksum    : 16                : bigendian;
        payload     : (length - 8) * 8  : bitstring
    |} ->
    Some ({ source; destination; length; checksum }, payload)
  | {| _ |} -> None

let decode data =
  let open Core.Option.Monad_infix in
  decode_header data >>= fun (hdr, _) -> Some (hdr)

let expand data =
  let open Core.Option.Monad_infix in
  decode_header data >>= fun (_, payload) -> Some (payload)
