module Protocol : sig
  type t = 
    | TCP
    | UDP
    [@@deriving yojson]
end

module TCP  = NetML_Layer_IV_TCP
module UDP  = NetML_Layer_IV_UDP
