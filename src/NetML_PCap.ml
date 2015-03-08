open Bitstring
open Core.Std

module Format = struct
  type t =
    | Microsecond
    | Nanosecond
    | Invalid

  let of_int = function
    | 0xa1b2c3d4_l | 0xd4c3b2a1_l -> Microsecond
    | 0xa1b23c4d_l | 0x4d3cb2a1_l -> Nanosecond
    | _ -> Invalid

  let to_int endian = function
    | Microsecond -> if endian = Bitstring.BigEndian    then 0xa1b2c3d4_l else 0xd4c3b2a1_l
    | Nanosecond  -> if endian = Bitstring.LittleEndian then 0xa1b23c4d_l else 0x4d3cb2a1_l
    | Invalid     -> 0_l

  let to_string = function
    | Microsecond -> "us"
    | Nanosecond  -> "ns"
    | Invalid     -> ""
end

type t = {
  endian  : Bitstring.endian;
  format  : Format.t;
  version : (int * int);
  snaplen : int;
  data    : Bitstring.t
}

let endian_of = function
  | 0xa1b2c3d4_l | 0xa1b23c4d_l -> Bitstring.BigEndian
  | 0xd4c3b2a1_l | 0x4d3cb2a1_l -> Bitstring.LittleEndian
  | _ -> assert false

let open_file fn =
  let bs = Bitstring.bitstring_of_file fn in
  match%bitstring bs with
  | {|  ((0xa1b2c3d4_l | 0xa1b23c4d_l | 0xd4c3b2a1_l | 0x4d3cb2a1_l) as magic) : 32;
        major   : 16 : endian (endian_of magic);
        minor   : 16 : endian (endian_of magic);
        _       : 32 : endian (endian_of magic); (* TZ *)
        0_l     : 32 : endian (endian_of magic);
        snaplen : 32 : endian (endian_of magic);
        _       : 32 : endian (endian_of magic); (* Network *)
        payload : -1 : bitstring
    |} ->
      let fmt = Format.of_int magic in
      if fmt = Format.Invalid then
        None
      else begin
        printf "PCAP: %s\n" fn;
        printf "Format: %s\n" (Format.to_string fmt);
        printf "Version: (%d, %d)\n" major minor;
        printf "Snapshot length: %ld B\n" snaplen;
        Some {
          endian = endian_of magic; format = fmt; version = (major, minor);
          snaplen = Int32.to_int_exn snaplen; data = payload
        }
      end
  | {| _ |} -> None

let rec iter f endian snaplen cnt pkt =
  match%bitstring pkt with
  | {|  sec       : 32                          : endian (endian);
        usec      : 32                          : endian (endian);
        len       : 32                          : endian (endian);
        _         : 32                          : endian (endian);
        data      : (Int32.to_int_exn len) * 8  : bitstring;
        payload   : -1                          : bitstring
    |} ->
      let ilen = (Int32.to_int_exn len) in
      if ilen > snaplen then
        printf
        "Packet #%d: length (%d) larger than the maximum snapshot length (%d)" cnt ilen snaplen
      else
        let ns = (Int32.to_int_exn sec) * 1_000_000_000 + (Int32.to_int_exn usec) * 1_000 in
        f ~ts:ns ~data:data ~len:ilen;
        iter f endian snaplen (cnt + 1) payload
  | {| _ |} -> ()

let iter ~f file =
  let open Format in
  match file.format with
  | Microsecond | Nanosecond  -> iter f file.endian file.snaplen 1 file.data
  | Invalid                   -> ()
