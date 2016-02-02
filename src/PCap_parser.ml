open Core.Std
open NetML

let spec =
  let open Command.Spec in
  empty
  +> flag "-t" no_arg ~doc:" show packet time stamp"
  +> flag "-s" no_arg ~doc:" show packet size"
  +> flag "-E" no_arg ~doc:" show Ethernet information"
  +> anon ("filename" %: file)

let process pkt l2_proto l2 =
  let open Option.Monad_infix in
  let json = PCap.Packet.Header.to_yojson (PCap.Packet.header pkt) in
  Printf.printf "%s\n" (Yojson.Safe.to_string json);
  ignore (
    Layer.II.decode (l2_proto, l2)  >>= fun (l3_proto, l3) ->
    Layer.III.decode (l3_proto, l3) >>= fun (l4_proto, l4) ->
    None
  )

let operation show_ts show_sz fn =
  let open Option.Monad_infix in
  ignore (
    PCap.open_file fn >>= fun pcap ->
    let json = PCap.Header.to_yojson (PCap.header pcap) in
    Printf.printf "%s\n" (Yojson.Safe.to_string json);
    PCap.iter process pcap
  )

let command =
  Command.basic
  ~summary:"Parse PCAP files"
  ~readme:(fun () -> "More detailed information")
  spec
  (fun show_ts show_sz _ fn () -> operation show_ts show_sz fn)

let () =
  Command.run ~version:"1.0" ~build_info:"RWO" command
