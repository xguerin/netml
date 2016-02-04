let spec =
  let open Core.Command.Spec in
  empty
  +> flag "-t" no_arg ~doc:" show packet time stamp"
  +> flag "-s" no_arg ~doc:" show packet size"
  +> flag "-p" no_arg ~doc:" prettify JSON output"
  +> anon ("filename" %: file)

let decode pkt l2 =
  let open Core.Option.Monad_infix in
  let open NetML.Layer in
  II.decode l2  >>= fun l2_hdr ->
  II.expand l2  >>= fun l3 ->
  III.decode l3 >>= fun l3_hdr ->
  III.expand l3 >>= fun l4 ->
  IV.decode l4  >>= fun l4_hdr ->
  Some (`List [
      II.header_to_yojson l2_hdr;
      III.header_to_yojson l3_hdr;
      IV.header_to_yojson l4_hdr;
    ])

let process pretty pkt l2 =
  match decode pkt l2 with
  | Some (json) ->
    if pretty then
      Printf.printf "%s\n" (Yojson.Safe.pretty_to_string json)
    else
      Printf.printf "%s\n" (Yojson.Safe.to_string json)
  | None -> ()

let operation show_ts show_sz pretty fn =
  match NetML.PCap.open_file fn with
  | Some (pcap) ->
    NetML.PCap.iter (process pretty) pcap
  | None -> ()

let command =
  Core.Command.basic
  ~summary:"Parse PCAP files"
  ~readme:(fun () -> "More detailed information")
  spec
  (fun show_ts show_sz pretty fn () -> operation show_ts show_sz pretty fn)

let () =
  Core.Command.run ~version:"1.0" ~build_info:"RWO" command
