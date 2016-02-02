open Bitstring
open Core.Std
open NetML_Layer_II

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

let protocol_of_int = function
  | 0_l                               -> Some Protocol.Null
  | 1_l                               -> Some Protocol.Ethernet
  | 3_l                               -> Some Protocol.AX25
  | 6_l                               -> Some Protocol.IEEE802_5
  | 7_l                               -> Some Protocol.ARCNet_BSD
  | 8_l                               -> Some Protocol.SLIP
  | 9_l                               -> Some Protocol.PPP
  | 10_l                              -> Some Protocol.FDDI
  | 50_l                              -> Some Protocol.PPP_HDLC
  | 51_l                              -> Some Protocol.PPP_Ether
  | 100_l                             -> Some Protocol.ATM_RFC1483
  | 101_l                             -> Some Protocol.Raw
  | 104_l                             -> Some Protocol.C_HDLC
  | 105_l                             -> Some Protocol.IEEE802_11
  | 107_l                             -> Some Protocol.FRelay
  | 108_l                             -> Some Protocol.Loop
  | 113_l                             -> Some Protocol.Linux_SLL
  | 114_l                             -> Some Protocol.LTalk
  | 117_l                             -> Some Protocol.PFLog
  | 119_l                             -> Some Protocol.IEEE802_11_Prism
  | 122_l                             -> Some Protocol.IP_over_FC
  | 123_l                             -> Some Protocol.SunATM
  | 127_l                             -> Some Protocol.IEEE802_11_RadioTAP
  | 129_l                             -> Some Protocol.ARCNET_Linux
  | 138_l                             -> Some Protocol.Apple_IP_Over_IEEE1394
  | 139_l                             -> Some Protocol.MTP2_With_PHDR
  | 140_l                             -> Some Protocol.MTP2
  | 141_l                             -> Some Protocol.MTP3
  | 142_l                             -> Some Protocol.SCCP
  | 143_l                             -> Some Protocol.DOCSIS
  | 144_l                             -> Some Protocol.Linux_IRDA
  | i when (i >= 147_l && i <= 162_l) -> Some Protocol.User0_LinkType_User15
  | 163_l                             -> Some Protocol.IEEE802_11_AVS
  | 165_l                             -> Some Protocol.BACNET_MS_TP
  | 166_l                             -> Some Protocol.PPP_PPPD
  | 169_l                             -> Some Protocol.GPRS_LLC
  | 170_l                             -> Some Protocol.GPF_T
  | 171_l                             -> Some Protocol.GPF_F
  | 177_l                             -> Some Protocol.Linux_LAPD
  | 187_l                             -> Some Protocol.Bluetooth_HCI_H4
  | 189_l                             -> Some Protocol.USB_Linux
  | 192_l                             -> Some Protocol.PPI
  | 195_l                             -> Some Protocol.IEEE802_15_4
  | 196_l                             -> Some Protocol.SITA
  | 197_l                             -> Some Protocol.ERF
  | 201_l                             -> Some Protocol.Bluetooth_HCI_H4_With_PHDR
  | 202_l                             -> Some Protocol.AX25_KISS
  | 203_l                             -> Some Protocol.LAPD
  | 204_l                             -> Some Protocol.PPP_With_DIR
  | 205_l                             -> Some Protocol.C_HDLC_With_DIR
  | 206_l                             -> Some Protocol.FRelay_With_DIR
  | 209_l                             -> Some Protocol.IPMB_Linux
  | 215_l                             -> Some Protocol.IEEE802_15_4_NONASK_PHY
  | 220_l                             -> Some Protocol.USB_Linux_MMapped
  | 224_l                             -> Some Protocol.FC_2
  | 225_l                             -> Some Protocol.FC_2_WITH_FRAME_DELIMS
  | 226_l                             -> Some Protocol.IPNET
  | 227_l                             -> Some Protocol.CAN_SOCKETCAN
  | 228_l                             -> Some Protocol.IPv4
  | 229_l                             -> Some Protocol.IPv6
  | 230_l                             -> Some Protocol.IEEE802_15_4_NOFCS
  | 231_l                             -> Some Protocol.DBUS
  | 235_l                             -> Some Protocol.DVB_CI
  | 236_l                             -> Some Protocol.MUX27010
  | 237_l                             -> Some Protocol.STANAG_5066_D_PDU
  | 239_l                             -> Some Protocol.NFLOG
  | 240_l                             -> Some Protocol.NetAnalyzer
  | 241_l                             -> Some Protocol.NetAnalyzer_Transparent
  | 242_l                             -> Some Protocol.IPoIB
  | 243_l                             -> Some Protocol.MPEG_2_TS
  | 244_l                             -> Some Protocol.NG40
  | 245_l                             -> Some Protocol.NFC_LLCP
  | 247_l                             -> Some Protocol.Infiniband
  | 248_l                             -> Some Protocol.SCTP
  | 249_l                             -> Some Protocol.USBPCAP
  | 250_l                             -> Some Protocol.RTAC_Serial
  | 251_l                             -> Some Protocol.Bluetooth_LE_LL
  | 253_l                             -> Some Protocol.NetLink
  | 254_l                             -> Some Protocol.Bluetooth_Linux_Monitor
  | 255_l                             -> Some Protocol.Bluetooth_BREDR_BB
  | 256_l                             -> Some Protocol.Bluetooth_LE_LL_WITH_PHDR
  | 257_l                             -> Some Protocol.ProfiBus_DL
  | 258_l                             -> Some Protocol.PKTAP
  | 259_l                             -> Some Protocol.EPON
  | 260_l                             -> Some Protocol.IPMI_HPM_2
  | 261_l                             -> Some Protocol.ZWAVE_R1_R2
  | 262_l                             -> Some Protocol.ZWAVE_R3
  | 263_l                             -> Some Protocol.WattStopper_DLM
  | 264_l                             -> Some Protocol.ISO_14443
  | v                                 -> None

module Format = struct
  type t =
    | Microsecond
    | Nanosecond
    [@@deriving yojson]
  let of_int v =
    if v = 0xa1b2c3d4_l || v =  0xd4c3b2a1_l then
      Microsecond
    else (* 0xa1b23c4d_l || 0x4d3cb2a1_l *)
      Nanosecond
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
  let to_endian v =
    if v = 0xa1b2c3d4_l || v = 0xa1b23c4d_l then
      Endian.Big
    else (* 0xd4c3b2a1_l || 0x4d3cb2a1_l *)
      Endian.Little
  let to_bitstring_endian v =
    if v = 0xa1b2c3d4_l || v = 0xa1b23c4d_l then
      Bitstring.BigEndian
    else (* 0xd4c3b2a1_l || 0x4d3cb2a1_l *)
      Bitstring.LittleEndian
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
    nettype : Protocol.t;
  } [@@deriving yojson]
end

module Packet = struct
  module Header = struct
    type t = {
      sec       : Int32.t;
      rsec      : Int32.t;
      incl_len  : Int32.t;
      orig_len  : Int32.t;
      nettype   : Protocol.t;
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
    begin match protocol_of_int network with
    | Some (net) ->
      let hdr = {
        endian = Format.to_endian magic;
        format = fmt; version = (major, minor);
        snaplen = snaplen;
        nettype = net
      } in
      Some (hdr, payload)
    | None -> None
    end
  | {| _ |} -> None

let header (hdr, _) = hdr

let next_packet (hdr, pld) =
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
      None
    else
      let open Packet.Header in
      let phdr = {
        sec = sec;
        rsec = rsec;
        incl_len = incl_len;
        orig_len = orig_len;
        nettype = hdr.nettype
      } in
      let pkt = begin match hdr.format with
        | Format.Microsecond -> Packet.create_usec phdr data
        | Format.Nanosecond -> Packet.create_nsec phdr data
      end in
      Some (pkt, (hdr, payload))
  | {| _ |} -> None
