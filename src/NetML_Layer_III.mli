module IPv4 = NetML_Layer_III_IPv4

module Protocol : sig
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

val decode : t -> header option

val expand : t -> NetML_Layer_IV.t option
