open Core.Std
open NetML.PCap
open NetML.Layer

let spec =
  let open Command.Spec in
  empty
  +> flag "-t" no_arg ~doc:" show packet time stamp"
  +> flag "-s" no_arg ~doc:" show packet size"
  +> flag "-E" no_arg ~doc:" show Ethernet information"
  +> flag "-I" no_arg ~doc:" show IPv4 information"
  +> flag "-U" no_arg ~doc:" show UDP information"
  +> flag "-T" no_arg ~doc:" show TCP information"
  +> anon ("filename" %: file)

let process_l3 proto =
  let open NetML.Layer.IPv4.Protocol in
  match proto with
  | UDP udp ->
      printf "%s\n" (NetML.Layer.UDP.to_string udp);
      Some proto
  | TCP tcp ->
      printf "%s\n" (NetML.Layer.TCP.to_string tcp);
      Some proto
  | Unsupported -> printf " ??? \n"; None

let process_l2 proto =
  let open NetML.Layer.Ethernet.Protocol in
  match proto with
  | IPv4 ip ->
      printf "%s " (NetML.Layer.IPv4.to_string ip);
      process_l3 ip.IPv4.protocol
  | _ -> printf " ??? \n"; None

let rec process_l1 proto =
  let open NetML.Layer.Ethernet.Protocol in
  match proto with
  | Length _    -> printf " ??? \n"; None
  | Unsupported -> printf " ??? \n"; None
  | IPv4 _      -> process_l2 proto
  | VLAN (_, v) -> printf "VLAN "; process_l1 v

let iterator show_ts show_sz ~ts ~data ~len =
  if show_ts then printf "%d " ts;
  if show_sz then printf "(%4d B) " len;
  let open NetML.Layer.Ethernet in
  let open NetML.Layer.Ethernet.Protocol in
  let open Option.Monad_infix in
  ignore (
    NetML.Layer.Ethernet.decode data >>= (fun eth ->
        printf "%s " (NetML.Layer.Ethernet.to_string eth);
        process_l1 eth.protocol)
  )

let operation show_ts show_sz fn =
  let open NetML.PCap in
  match open_file fn with
  | Some pcap ->
    let json = Header.to_yojson (header pcap) in
    Printf.printf "%s\n" (Yojson.Safe.to_string json);
    iter (fun pkt ->
        let json = Packet.Header.to_yojson (Packet.header pkt) in
        let ts = Int64.to_string (Packet.timestamp_ns pkt) in
        Printf.printf "%s %s\n" ts (Yojson.Safe.to_string json))
      pcap
  | None ->
    Printf.printf "ERROR: %s is not a supported PCAP type\n" fn

let command =
  Command.basic
  ~summary:"Parse PCAP files"
  ~readme:(fun () -> "More detailed information")
  spec
  (fun show_ts show_sz _ _ _ _ fn () ->
     operation show_ts show_sz fn)

let () =
  Command.run ~version:"1.0" ~build_info:"RWO" command
