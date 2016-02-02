module Protocol : sig
  type t = IPv4 [@@deriving yojson]
end

module IPv4 = NetML_Layer_III_IPv4
