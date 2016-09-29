type t = {
  source      : int;
  destination : int;
  seqnum      : Int32.t;
  acknum      : Int32.t;
  checksum    : int;
} [@@deriving yojson]

let decode_header data =
  match%bitstring data with
  | {|  source      : 16 : bigendian;
        destination : 16 : bigendian;
        seqnum      : 32 : bigendian;
        acknum      : 32 : bigendian;
        offset      : 4;
        _           : 3;
        _           : 1; (* ns *)
        _           : 1; (* cwr *)
        _           : 1; (* ece *)
        _           : 1; (* urg *)
        _           : 1; (* ack *)
        _           : 1; (* psh *)
        _           : 1; (* rst *)
        _           : 1; (* syn *)
        _           : 1; (* fin *)
        _           : 16 : bigendian; (* winsize *)
        checksum    : 16 : bigendian;
        _           : 16 : bigendian; (* urgptr *)
        _           : (offset - 5) * 32 : bitstring; (* options *)
        payload     : -1 : bitstring
    |} ->
    let open Core.Std in
    Some ({ source; destination; seqnum; acknum; checksum }, payload)
  | {| _ |} -> None

let decode data =
  let open Core_kernel.Option.Monad_infix in
  decode_header data >>= fun (hdr, _) -> Some (hdr)

let expand data =
  let open Core_kernel.Option.Monad_infix in
  decode_header data >>= fun (_, payload) -> Some (payload)
