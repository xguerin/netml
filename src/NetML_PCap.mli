open Bitstring

exception Bad_network of Int32.t
exception Bad_format of Int32.t

(** PCap endianness type *)
module Endian : sig
  type t =
    | Big
    | Little
    | Native
    [@@deriving yojson]
  val to_bitstring_endian : t -> Bitstring.endian
  val of_bitstring_endian : Bitstring.endian -> t
end

module Network : sig
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
  val of_int : Int32.t -> t
end

(** PCap format definition *)
module Format : sig
  type t [@@deriving yojson]
  val of_int : Int32.t -> t
  val to_int : Endian.t -> t -> Int32.t
  val to_endian : Int32.t -> Endian.t
  val to_bitstring_endian : Int32.t -> Bitstring.endian
  val to_string : t -> string
end

(** PCap global and packet header definitions *)
module Header : sig
  type t = {
    endian  : Endian.t;
    format  : Format.t;
    version : (int * int);
    snaplen : Int32.t;
    nettype : Network.t;
  } [@@deriving yojson]
end

(** PCap packet definition *)
module Packet : sig
  module Header : sig
    type t = {
      sec       : Int32.t;
      rsec      : Int32.t;
      incl_len  : Int32.t;
      orig_len  : Int32.t;
      nettype   : Network.t;
    } [@@deriving yojson]
  end
  type 'a t
  type usec_format
  type nsec_format
  val create_usec : Header.t -> bitstring -> usec_format t
  val create_nsec : Header.t -> bitstring -> nsec_format t
  val header : 'a t -> Header.t
  val timestamp_ns : usec_format t -> Int64.t
  val timestamp_ns : nsec_format t -> Int64.t
end

(** PCap file type *)
type t

(** Open a PCAP file *)
val open_file : string -> t option

(** Retrieve the global header of a PCAP file *)
val header : t -> Header.t

(** Iterate over a PCAP file *)
val iter :  ('a Packet.t -> unit) -> t -> unit
