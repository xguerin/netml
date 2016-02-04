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

type header =
  | TCP of TCP.t
  | UDP of UDP.t
  | Unsupported
  [@@deriving yojson]

let decode (proto, data) =
  let open Core.Option.Monad_infix in
  match proto with
  | Protocol.TCP  -> TCP.decode data >>= fun hdr -> Some (TCP (hdr))
  | Protocol.UDP  -> UDP.decode data >>= fun hdr -> Some (UDP (hdr))
  | _             -> None

let expand (proto, data) =
  match proto with
  | Protocol.TCP  -> TCP.expand data
  | Protocol.UDP  -> UDP.expand data
  | _             -> None

