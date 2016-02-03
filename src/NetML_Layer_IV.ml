module TCP  = NetML_Layer_IV_TCP
module UDP  = NetML_Layer_IV_UDP

module Protocol = struct
  type t =
    | TCP
    | UDP
    | Unsupported
    [@@deriving yojson]
end

type t = (Protocol.t * Bitstring.t)

let expand (proto, data) =
  match proto with
  | Protocol.TCP  -> None
  | Protocol.UDP  -> None
  | _             -> None

