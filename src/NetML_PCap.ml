open Bitstring
open Core.Std

exception Bad_network of Int32.t
exception Bad_format of Int32.t

module Endian = struct
  type t =
    | Big
    | Little
    | Native
    [@@deriving yojson]
  let to_bitstring_endian = function
    | Big -> Bitstring.BigEndian
    | Little -> Bitstring.LittleEndian
    | Native -> Bitstring.NativeEndian
  let of_bitstring_endian = function
    | Bitstring.BigEndian -> Big
    | Bitstring.LittleEndian -> Little
    | Bitstring.NativeEndian -> Native
end

module Network = struct
  type t =
    | Null
    | Ethernet
    | AX25
    | IEEE802_5
    | ARCNet_BSD
    | SLIP
    | PPP
    | FDDI
    | PPP_HDLC
    | PPP_Ether
    | ATM_RFC1483
    | Raw
    | C_HDLC
    | IEEE802_11
    | FRelay
    | Loop
    | Linux_SLL
    | LTalk
    | PFLog
    | IEEE802_11_Prism
    | IP_over_FC
    | SunATM
    | IEEE802_11_RadioTAP
    | ARCNET_Linux
    | Apple_IP_Over_IEEE1394
    | MTP2_With_PHDR
    | MTP2
    | MTP3
    | SCCP
    | DOCSIS
    | Linux_IRDA
    | User0_LinkType_User15
    | IEEE802_11_AVS
    | BACNET_MS_TP
    | PPP_PPPD
    | GPRS_LLC
    | GPF_T
    | GPF_F
    | Linux_LAPD
    | Bluetooth_HCI_H4
    | USB_Linux
    | PPI
    | IEEE802_15_4
    | SITA
    | ERF
    | Bluetooth_HCI_H4_With_PHDR
    | AX25_KISS
    | LAPD
    | PPP_With_DIR
    | C_HDLC_With_DIR
    | FRelay_With_DIR
    | IPMB_Linux
    | IEEE802_15_4_NONASK_PHY
    | USB_Linux_MMapped
    | FC_2
    | FC_2_WITH_FRAME_DELIMS
    | IPNET
    | CAN_SOCKETCAN
    | IPv4
    | IPv6
    | IEEE802_15_4_NOFCS
    | DBUS
    | DVB_CI
    | MUX27010
    | STANAG_5066_D_PDU
    | NFLOG
    | NetAnalyzer
    | NetAnalyzer_Transparent
    | IPoIB
    | MPEG_2_TS
    | NG40
    | NFC_LLCP
    | Infiniband
    | SCTP
    | USBPCAP
    | RTAC_Serial
    | Bluetooth_LE_LL
    | NetLink
    | Bluetooth_Linux_Monitor
    | Bluetooth_BREDR_BB
    | Bluetooth_LE_LL_WITH_PHDR
    | ProfiBus_DL
    | PKTAP
    | EPON
    | IPMI_HPM_2
    | ZWAVE_R1_R2
    | ZWAVE_R3
    | WattStopper_DLM
    | ISO_14443
    [@@deriving yojson]
  let of_int = function
    | 0_l                               -> Null
    | 1_l                               -> Ethernet
    | 3_l                               -> AX25
    | 6_l                               -> IEEE802_5
    | 7_l                               -> ARCNet_BSD
    | 8_l                               -> SLIP
    | 9_l                               -> PPP
    | 10_l                              -> FDDI
    | 50_l                              -> PPP_HDLC
    | 51_l                              -> PPP_Ether
    | 100_l                             -> ATM_RFC1483
    | 101_l                             -> Raw
    | 104_l                             -> C_HDLC
    | 105_l                             -> IEEE802_11
    | 107_l                             -> FRelay
    | 108_l                             -> Loop
    | 113_l                             -> Linux_SLL
    | 114_l                             -> LTalk
    | 117_l                             -> PFLog
    | 119_l                             -> IEEE802_11_Prism
    | 122_l                             -> IP_over_FC
    | 123_l                             -> SunATM
    | 127_l                             -> IEEE802_11_RadioTAP
    | 129_l                             -> ARCNET_Linux
    | 138_l                             -> Apple_IP_Over_IEEE1394
    | 139_l                             -> MTP2_With_PHDR
    | 140_l                             -> MTP2
    | 141_l                             -> MTP3
    | 142_l                             -> SCCP
    | 143_l                             -> DOCSIS
    | 144_l                             -> Linux_IRDA
    | i when (i >= 147_l && i <= 162_l) -> User0_LinkType_User15
    | 163_l                             -> IEEE802_11_AVS
    | 165_l                             -> BACNET_MS_TP
    | 166_l                             -> PPP_PPPD
    | 169_l                             -> GPRS_LLC
    | 170_l                             -> GPF_T
    | 171_l                             -> GPF_F
    | 177_l                             -> Linux_LAPD
    | 187_l                             -> Bluetooth_HCI_H4
    | 189_l                             -> USB_Linux
    | 192_l                             -> PPI
    | 195_l                             -> IEEE802_15_4
    | 196_l                             -> SITA
    | 197_l                             -> ERF
    | 201_l                             -> Bluetooth_HCI_H4_With_PHDR
    | 202_l                             -> AX25_KISS
    | 203_l                             -> LAPD
    | 204_l                             -> PPP_With_DIR
    | 205_l                             -> C_HDLC_With_DIR
    | 206_l                             -> FRelay_With_DIR
    | 209_l                             -> IPMB_Linux
    | 215_l                             -> IEEE802_15_4_NONASK_PHY
    | 220_l                             -> USB_Linux_MMapped
    | 224_l                             -> FC_2
    | 225_l                             -> FC_2_WITH_FRAME_DELIMS
    | 226_l                             -> IPNET
    | 227_l                             -> CAN_SOCKETCAN
    | 228_l                             -> IPv4
    | 229_l                             -> IPv6
    | 230_l                             -> IEEE802_15_4_NOFCS
    | 231_l                             -> DBUS
    | 235_l                             -> DVB_CI
    | 236_l                             -> MUX27010
    | 237_l                             -> STANAG_5066_D_PDU
    | 239_l                             -> NFLOG
    | 240_l                             -> NetAnalyzer
    | 241_l                             -> NetAnalyzer_Transparent
    | 242_l                             -> IPoIB
    | 243_l                             -> MPEG_2_TS
    | 244_l                             -> NG40
    | 245_l                             -> NFC_LLCP
    | 247_l                             -> Infiniband
    | 248_l                             -> SCTP
    | 249_l                             -> USBPCAP
    | 250_l                             -> RTAC_Serial
    | 251_l                             -> Bluetooth_LE_LL
    | 253_l                             -> NetLink
    | 254_l                             -> Bluetooth_Linux_Monitor
    | 255_l                             -> Bluetooth_BREDR_BB
    | 256_l                             -> Bluetooth_LE_LL_WITH_PHDR
    | 257_l                             -> ProfiBus_DL
    | 258_l                             -> PKTAP
    | 259_l                             -> EPON
    | 260_l                             -> IPMI_HPM_2
    | 261_l                             -> ZWAVE_R1_R2
    | 262_l                             -> ZWAVE_R3
    | 263_l                             -> WattStopper_DLM
    | 264_l                             -> ISO_14443
    | v                                 -> raise (Bad_network v)
end

module Format = struct
  type t =
    | Microsecond
    | Nanosecond
    [@@deriving yojson]
  let of_int = function
    | 0xa1b2c3d4_l | 0xd4c3b2a1_l -> Microsecond
    | 0xa1b23c4d_l | 0x4d3cb2a1_l -> Nanosecond
    | v                           -> raise (Bad_format v)
  let to_int endian = function
    | Microsecond ->
      begin match endian with
        | Endian.Big -> 0xa1b2c3d4_l
        | _          -> 0xd4c3b2a1_l
      end
    | Nanosecond  ->
      begin match endian with
        | Endian.Little -> 0xa1b23c4d_l
        | _             -> 0x4d3cb2a1_l
      end
  let to_endian = function
    | 0xa1b2c3d4_l | 0xa1b23c4d_l -> Endian.Big
    | 0xd4c3b2a1_l | 0x4d3cb2a1_l -> Endian.Little
    | v                           -> raise (Bad_format v)
  let to_bitstring_endian = function
    | 0xa1b2c3d4_l | 0xa1b23c4d_l -> Bitstring.BigEndian
    | 0xd4c3b2a1_l | 0x4d3cb2a1_l -> Bitstring.LittleEndian
    | v                           -> raise (Bad_format v)
  let to_string = function
    | Microsecond -> "us"
    | Nanosecond  -> "ns"
end

module Header = struct
  type t = {
    endian  : Endian.t;
    format  : Format.t;
    version : (int * int);
    snaplen : Int32.t;
    nettype : Network.t;
  } [@@deriving yojson]
end

module Packet = struct
  module Header = struct
    type t = {
      sec       : Int32.t;
      rsec      : Int32.t;
      incl_len  : Int32.t;
      orig_len  : Int32.t;
      nettype   : Network.t;
    } [@@deriving yojson]
  end
  type 'a t = (Header.t * bitstring)
  type usec_format
  type nsec_format
  let create_usec hdr data = (hdr, data)
  let create_nsec hdr data = (hdr, data)
  let header (hdr, _) = hdr
  let timestamp_ns : usec_format t -> Int64.t = fun (hdr, _) ->
    let open Int64 in
    let open Header in
    (Int64.of_int32 hdr.sec) * (Int64.of_int 1_000_000_000) +
    (Int64.of_int32 hdr.rsec) * (Int64.of_int 1_000)
  let timestamp_ns : nsec_format t -> Int64.t = fun (hdr, _) ->
    let open Int64 in
    let open Header in
    (Int64.of_int32 hdr.sec) * (Int64.of_int 1_000_000_000) +
    (Int64.of_int32 hdr.rsec)
end

type t = (Header.t * bitstring)

let open_file fn =
  let bs = Bitstring.bitstring_of_file fn in
  match%bitstring bs with
  | {|  ((0xa1b2c3d4_l | 0xa1b23c4d_l | 0xd4c3b2a1_l | 0x4d3cb2a1_l) as magic) : 32;
        major   : 16 : endian (Format.to_bitstring_endian magic);
        minor   : 16 : endian (Format.to_bitstring_endian magic);
        _       : 32 : endian (Format.to_bitstring_endian magic); (* TZ *)
        0_l     : 32 : endian (Format.to_bitstring_endian magic);
        snaplen : 32 : endian (Format.to_bitstring_endian magic);
        network : 32 : endian (Format.to_bitstring_endian magic);
        payload : -1 : bitstring
    |} ->
    let open Header in
    let fmt = Format.of_int magic in
    let hdr = {
      endian = Format.to_endian magic;
      format = fmt; version = (major, minor);
      snaplen = snaplen;
      nettype = Network.of_int network
    } in
    Some (hdr, payload)
  | {| _ |} -> None

let header (hdr, _) = hdr

let rec iter fn cnt (hdr, pld) =
  let open Header in
  let endian = Endian.to_bitstring_endian hdr.endian in
  match%bitstring pld with
  | {| sec       : 32                               : endian (endian);
       rsec      : 32                               : endian (endian);
       incl_len  : 32                               : endian (endian);
       orig_len  : 32                               : endian (endian);
       data      : (Int32.to_int_exn incl_len) * 8  : bitstring;
       payload   : -1                               : bitstring
       |} ->
    if incl_len > hdr.snaplen then
      printf
        "Packet #%d: length (%s) larger than the maximum snapshot length (%s)"
        cnt (Int32.to_string incl_len) (Int32.to_string hdr.snaplen)
    else
      let open Packet.Header in
      let phdr = {
        sec = sec;
        rsec = rsec;
        incl_len = incl_len;
        orig_len = orig_len;
        nettype = hdr.nettype
      } in
      begin match hdr.format with
        | Format.Microsecond -> fn (Packet.create_usec phdr data)
        | Format.Nanosecond -> fn (Packet.create_nsec phdr data)
      end ;
      iter fn (cnt + 1) (hdr, payload)
  | {| _ |} -> ()

let iter fn = iter fn 0
