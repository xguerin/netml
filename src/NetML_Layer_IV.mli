module TCP  = NetML_Layer_IV_TCP
module UDP  = NetML_Layer_IV_UDP

module Protocol : sig
  type t = 
    | TCP
    | UDP
    | Unsupported
    [@@deriving yojson]
end

type t = (Protocol.t * Bitstring.t)

val expand : t -> Bitstring.t option
