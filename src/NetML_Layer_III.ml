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
  match proto with
  | Protocol.IPv4 ->
    begin match IPv4.decode data with
      | Some (hdr)  -> Some (IPv4 (hdr))
      | None        -> None
    end
  | _ -> None

let expand (proto, data) =
  match proto with
  | Protocol.IPv4 -> IPv4.expand data
  | Protocol.Unsupported -> None
