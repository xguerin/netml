open Core.Std
open NetML

let spec =
  let open Command.Spec in
  empty
  +> flag "-t" no_arg ~doc:" show packet time stamp"
  +> flag "-s" no_arg ~doc:" show packet size"
  +> anon ("filename" %: file)

let process acc pkt l2 =
  let open Option.Monad_infix in
  let open Layer in
  II.decode l2  >>= fun l2_hdr ->
  II.expand l2  >>= fun l3 ->
  III.decode l3 >>= fun l3_hdr ->
  III.expand l3 >>= fun l4 ->
  Some (`List [
      II.header_to_yojson l2_hdr;
      III.header_to_yojson l3_hdr
    ])

let operation show_ts show_sz fn =
  let open Option.Monad_infix in
  ignore (
    PCap.open_file fn >>= fun pcap ->
    PCap.fold_left process (Some (`List ([]))) pcap >>= fun res ->
    Printf.printf "%s\n" (Yojson.Safe.pretty_to_string res);
    None
  )

let command =
  Command.basic
  ~summary:"Parse PCAP files"
  ~readme:(fun () -> "More detailed information")
  spec
  (fun show_ts show_sz fn () -> operation show_ts show_sz fn)

let () =
  Command.run ~version:"1.0" ~build_info:"RWO" command
