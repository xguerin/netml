open Bitstring
open Core.Std

exception Bad_format of int32

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
        _       : 32 : endian (Format.to_bitstring_endian magic); (* Network *)
        payload : -1 : bitstring
    |} ->
    let open Header in
    let fmt = Format.of_int magic in
    let hdr = {
      endian = Format.to_endian magic;
      format = fmt; version = (major, minor);
      snaplen = snaplen
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
        orig_len = orig_len
      } in
      begin match hdr.format with
        | Format.Microsecond -> fn (Packet.create_usec phdr data)
        | Format.Nanosecond -> fn (Packet.create_nsec phdr data)
      end ;
      iter fn (cnt + 1) (hdr, payload)
  | {| _ |} -> ()

let iter fn = iter fn 0
