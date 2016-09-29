module Endian = struct

  type t =
    | Big
    | Little
    | Native
  [@@deriving yojson]

  let to_bitstring_endian = function
    | Big    -> Bitstring.BigEndian
    | Little -> Bitstring.LittleEndian
    | Native -> Bitstring.NativeEndian
  ;;
  
  let of_bitstring_endian = function
    | Bitstring.BigEndian    -> Big
    | Bitstring.LittleEndian -> Little
    | Bitstring.NativeEndian -> Native
  ;;

end

let protocol_of_int proto =
  let open NetML_Layer_II.Protocol in
  match proto with
  | 0_l                               -> Some Null
  | 1_l                               -> Some Ethernet
  | 3_l                               -> Some AX25
  | 6_l                               -> Some IEEE802_5
  | 7_l                               -> Some ARCNet_BSD
  | 8_l                               -> Some SLIP
  | 9_l                               -> Some PPP
  | 10_l                              -> Some FDDI
  | 50_l                              -> Some PPP_HDLC
  | 51_l                              -> Some PPP_Ether
  | 100_l                             -> Some ATM_RFC1483
  | 101_l                             -> Some Raw
  | 104_l                             -> Some C_HDLC
  | 105_l                             -> Some IEEE802_11
  | 107_l                             -> Some FRelay
  | 108_l                             -> Some Loop
  | 113_l                             -> Some Linux_SLL
  | 114_l                             -> Some LTalk
  | 117_l                             -> Some PFLog
  | 119_l                             -> Some IEEE802_11_Prism
  | 122_l                             -> Some IP_over_FC
  | 123_l                             -> Some SunATM
  | 127_l                             -> Some IEEE802_11_RadioTAP
  | 129_l                             -> Some ARCNET_Linux
  | 138_l                             -> Some Apple_IP_Over_IEEE1394
  | 139_l                             -> Some MTP2_With_PHDR
  | 140_l                             -> Some MTP2
  | 141_l                             -> Some MTP3
  | 142_l                             -> Some SCCP
  | 143_l                             -> Some DOCSIS
  | 144_l                             -> Some Linux_IRDA
  | i when (i >= 147_l && i <= 162_l) -> Some User0_LinkType_User15
  | 163_l                             -> Some IEEE802_11_AVS
  | 165_l                             -> Some BACNET_MS_TP
  | 166_l                             -> Some PPP_PPPD
  | 169_l                             -> Some GPRS_LLC
  | 170_l                             -> Some GPF_T
  | 171_l                             -> Some GPF_F
  | 177_l                             -> Some Linux_LAPD
  | 187_l                             -> Some Bluetooth_HCI_H4
  | 189_l                             -> Some USB_Linux
  | 192_l                             -> Some PPI
  | 195_l                             -> Some IEEE802_15_4
  | 196_l                             -> Some SITA
  | 197_l                             -> Some ERF
  | 201_l                             -> Some Bluetooth_HCI_H4_With_PHDR
  | 202_l                             -> Some AX25_KISS
  | 203_l                             -> Some LAPD
  | 204_l                             -> Some PPP_With_DIR
  | 205_l                             -> Some C_HDLC_With_DIR
  | 206_l                             -> Some FRelay_With_DIR
  | 209_l                             -> Some IPMB_Linux
  | 215_l                             -> Some IEEE802_15_4_NONASK_PHY
  | 220_l                             -> Some USB_Linux_MMapped
  | 224_l                             -> Some FC_2
  | 225_l                             -> Some FC_2_WITH_FRAME_DELIMS
  | 226_l                             -> Some IPNET
  | 227_l                             -> Some CAN_SOCKETCAN
  | 228_l                             -> Some IPv4
  | 229_l                             -> Some IPv6
  | 230_l                             -> Some IEEE802_15_4_NOFCS
  | 231_l                             -> Some DBUS
  | 235_l                             -> Some DVB_CI
  | 236_l                             -> Some MUX27010
  | 237_l                             -> Some STANAG_5066_D_PDU
  | 239_l                             -> Some NFLOG
  | 240_l                             -> Some NetAnalyzer
  | 241_l                             -> Some NetAnalyzer_Transparent
  | 242_l                             -> Some IPoIB
  | 243_l                             -> Some MPEG_2_TS
  | 244_l                             -> Some NG40
  | 245_l                             -> Some NFC_LLCP
  | 247_l                             -> Some Infiniband
  | 248_l                             -> Some SCTP
  | 249_l                             -> Some USBPCAP
  | 250_l                             -> Some RTAC_Serial
  | 251_l                             -> Some Bluetooth_LE_LL
  | 253_l                             -> Some NetLink
  | 254_l                             -> Some Bluetooth_Linux_Monitor
  | 255_l                             -> Some Bluetooth_BREDR_BB
  | 256_l                             -> Some Bluetooth_LE_LL_WITH_PHDR
  | 257_l                             -> Some ProfiBus_DL
  | 258_l                             -> Some PKTAP
  | 259_l                             -> Some EPON
  | 260_l                             -> Some IPMI_HPM_2
  | 261_l                             -> Some ZWAVE_R1_R2
  | 262_l                             -> Some ZWAVE_R3
  | 263_l                             -> Some WattStopper_DLM
  | 264_l                             -> Some ISO_14443
  | v                                 -> None
;;

module Precision = struct

   type t =
    | Microsecond
    | Nanosecond
  [@@deriving yojson]

  let to_string = function
    | Microsecond -> "us"
    | Nanosecond  -> "ns"
  ;;

end

module Format = struct

  type t = Endian.t * Precision.t [@@deriving yojson]

  let decode = function
    | 0xa1b2c3d4_l -> Some (Endian.Big    , Precision.Microsecond)
    | 0xd4c3b2a1_l -> Some (Endian.Little , Precision.Microsecond)
    | 0xa1b23c4d_l -> Some (Endian.Big    , Precision.Nanosecond)
    | 0x4d3cb2a1_l -> Some (Endian.Little , Precision.Nanosecond)
    | _            -> None
  ;;

  let encode = function
    | (Endian.Big , Precision.Microsecond) -> 0xa1b2c3d4_l
    | (_          , Precision.Microsecond) -> 0xd4c3b2a1_l
    | (Endian.Big , Precision.Nanosecond)  -> 0xa1b23c4d_l
    | (_          , Precision.Nanosecond)  -> 0x4d3cb2a1_l
  ;;

end

module Header = struct

  type t = {
    format  : Format.t;
    version : (int * int);
    snaplen : Int32.t;
    nettype : NetML_Layer_II.Protocol.t;
  } [@@deriving yojson]

end

module Packet = struct

  module Header = struct

    type t = {
      sec       : Int32.t;
      rsec      : Int32.t;
      incl_len  : Int32.t;
      orig_len  : Int32.t;
    } [@@deriving yojson]

  end

  type t = Header.t * Bitstring.t

  let timestamp prec (hdr, _) =
    let open Core.Std.Int64 in
    let open Header in
    let nsecs = (of_int32 hdr.sec) * (of_int 1_000_000_000) in
    match prec with
    | Precision.Microsecond -> nsecs + (Int64.of_int32 hdr.rsec) * (Int64.of_int 1_000)
    | Precision.Nanosecond  -> nsecs + (Int64.of_int32 hdr.rsec)
  ;;

end

type t = (Header.t * Bitstring.t)

let open_file fn =
  let open Core_kernel.Option.Monad_infix in
  let open Header in
  let bs = Bitstring.bitstring_of_file fn in
  match%bitstring bs with
  | {| (0xa1b2c3d4_l | 0xa1b23c4d_l) as magic : 32
       ; major                                : 16 : bigendian
       ; minor                                : 16 : bigendian
       ; _                                    : 32
       ; 0_l                                  : 32
       ; snaplen                              : 32 : bigendian
       ; network                              : 32 : bigendian
       ; payload                              : -1 : bitstring
    |} ->
    Format.decode magic >>= fun format ->
    protocol_of_int network >>= fun nettype ->
    let hdr = {
      format;
      version = (major, minor);
      snaplen;
      nettype
    } in
    Some (hdr, payload)
  | {| (0xd4c3b2a1_l | 0x4d3cb2a1_l) as magic : 32
       ; major                                : 16 : littleendian
       ; minor                                : 16 : littleendian
       ; _                                    : 32
       ; 0_l                                  : 32
       ; snaplen                              : 32 : littleendian
       ; network                              : 32 : littleendian
       ; payload                              : -1 : bitstring
    |} ->
    Format.decode magic >>= fun format ->
    protocol_of_int network >>= fun nettype ->
    let hdr = {
      format;
      version = (major, minor);
      snaplen;
      nettype
    } in
    Some (hdr, payload)
  | {| _ |} -> None
;;

let header (hdr, _) =
  hdr
;;

let layerII ghdr (_, pdat) =
  (ghdr.Header.nettype, pdat)
;;

let decode_packet hdr pld =
  let open Header in
  let open Packet.Header in
  let (endian, _) = hdr.format in
  let bsendian = Endian.to_bitstring_endian endian in
  match%bitstring pld with
  | {| sec      : 32                          : endian (bsendian)
     ; rsec     : 32                          : endian (bsendian)
     ; incl_len : 32                          : endian (bsendian)
     ; orig_len : 32                          : endian (bsendian)
     ; data     : (Int32.to_int incl_len) * 8 : bitstring
     ; payload  : -1                          : bitstring
    |} ->
    if incl_len > hdr.snaplen then
      None
    else
      let phdr = { sec ; rsec ; incl_len ; orig_len } in
      Some ((phdr, data), payload)
  | {| _ |} -> None
;;

let rec iter fn (ghdr, pld) =
  match decode_packet ghdr pld with
  | Some (pkt, next) ->
    fn ghdr pkt;
    iter fn (ghdr, next)
  | None -> ()
;;

let rec fold_left fn acc (ghdr, pld) =
  match decode_packet ghdr pld with
  | Some (pkt, next) ->
    fold_left fn (fn acc ghdr pkt) (ghdr, next)
  | None -> acc
;;
