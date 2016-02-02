module Protocol : sig
  type t = 
    | TCP
    | UDP
end

module TCP  = NetML_Layer_IV_TCP
module UDP  = NetML_Layer_IV_UDP
