open Bitstring

module IPv4 = NetML_Layer_III_IPv4

module Protocol : sig
  type t =
    | IPv4
    | Unsupported
    [@@deriving yojson]
end

val decode : (Protocol.t * bitstring) -> (NetML_Layer_IV.Protocol.t * bitstring) option
