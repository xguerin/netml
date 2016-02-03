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
  type 'a t = (Header.t * Bitstring.t)
  type usec_format
  type nsec_format
  let create_usec hdr data = (hdr, data)
  let create_nsec hdr data = (hdr, data)
  let header (hdr, _) = hdr
  let timestamp_ns : usec_format t -> Int64.t = fun (hdr, _) ->
    let open Core.Std.Int64 in
    let open Header in
    (of_int32 hdr.sec) * (of_int 1_000_000_000) +
    (of_int32 hdr.rsec) * (of_int 1_000)
  let timestamp_ns : nsec_format t -> Int64.t = fun (hdr, _) ->
    let open Core.Std.Int64 in
    let open Header in
    (of_int32 hdr.sec) * (of_int 1_000_000_000) + (of_int32 hdr.rsec)
end

type t = (Header.t * Bitstring.t)

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

let rec iter fn (hdr, pld) =
  let open Core.Std in
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
      ()
    else
      let open Packet.Header in
      let phdr = {
        sec = sec;
        rsec = rsec;
        incl_len = incl_len;
        orig_len = orig_len
      } in
      let pkt = begin match hdr.format with
        | Format.Microsecond -> Packet.create_usec phdr data
        | Format.Nanosecond -> Packet.create_nsec phdr data
      end in
      fn pkt (hdr.Header.nettype, data);
      iter fn (hdr, payload)
  | {| _ |} -> ()

let rec fold_left fn acc (hdr, pld) =
  let open Core.Std in
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
      acc
    else
      let open Packet.Header in
      let phdr = {
        sec = sec;
        rsec = rsec;
        incl_len = incl_len;
        orig_len = orig_len
      } in
      let pkt = begin match hdr.format with
        | Format.Microsecond -> Packet.create_usec phdr data
        | Format.Nanosecond -> Packet.create_nsec phdr data
      end in
      fold_left fn (fn acc pkt (hdr.Header.nettype, data)) (hdr, payload)
  | {| _ |} -> acc

