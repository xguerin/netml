open Core.Std
open NetML

let spec =
  let open Command.Spec in
  empty
  +> flag "-t" no_arg ~doc:" show packet time stamp"
  +> flag "-s" no_arg ~doc:" show packet size"
  +> flag "-E" no_arg ~doc:" show Ethernet information"
  +> anon ("filename" %: file)

let build_json l2 l3 =
  let open Layer in
  let l2_json = ("ethernet", II.Ethernet.Header.to_yojson l2) in
  let l3_json = ("ipv4", III.IPv4.Header.to_yojson l3) in
  `Assoc [ l2_json; l3_json ]

let process acc pkt l2_proto l2 =
  let open Option.Monad_infix in
  let open Layer in
  II.Ethernet.header l2     >>= fun l2_hdr ->
  II.decode (l2_proto, l2)  >>= fun (l3_proto, l3) ->
  III.IPv4.header l3        >>= fun l3_hdr ->
  III.decode (l3_proto, l3) >>= fun (l4_proto, l4) ->
  Some (`List ([ build_json l2_hdr l3_hdr ]))


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
  (fun show_ts show_sz _ fn () -> operation show_ts show_sz fn)

let () =
  Command.run ~version:"1.0" ~build_info:"RWO" command
