module IPv4 = NetML_Layer_III_IPv4

module Protocol = struct

  type t =
    | IPv4
    | Unsupported
    [@@deriving yojson]

end

type t = (Protocol.t * Bitstring.t)

type header =
  | IPv4 of IPv4.t
  | Unsupported
  [@@deriving yojson]

let decode (proto, data) =
  let open Core_kernel.Option.Monad_infix in
  match proto with
  | Protocol.IPv4 -> IPv4.decode data >>| fun hdr -> IPv4 (hdr)
  | _             -> None

let expand (proto, data) =
  match proto with
  | Protocol.IPv4        -> IPv4.expand data
  | Protocol.Unsupported -> None
