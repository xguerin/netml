open Bitstring

module IPv4 = NetML_Layer_III_IPv4

module Protocol = struct
  type t =
    | IPv4
    | Unsupported
    [@@deriving yojson]
end

let decode (proto, data) =
  match proto with
  | Protocol.IPv4 -> IPv4.decode data
  | Protocol.Unsupported -> None

