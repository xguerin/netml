open Core.Std
open NetML.PCap
open NetML.Layer

let spec =
  let open Command.Spec in
  empty
  +> anon ("filename" %: file)

let process_l3 proto =
  let open NetML.Layer.IPv4.Protocol in
  match proto with
  | UDP udp ->
      printf "%s\n" (NetML.Layer.UDP.to_string udp);
      Some udp
  | Unsupported -> printf " ??? \n"; None
  | _ -> printf " ??? \n"; None

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

let iterator ~ts ~data ~len =
  printf "%d %4d " ts len;
  let open NetML.Layer.Ethernet in
  let open NetML.Layer.Ethernet.Protocol in
  let open Option.Monad_infix in
  ignore (
    NetML.Layer.Ethernet.decode data >>= fun eth ->
    printf "%s " (NetML.Layer.Ethernet.to_string eth);
    process_l1 eth.protocol
  )

let operation filename =
  match NetML.PCap.open_file filename with
  | Some pcap -> NetML.PCap.iter ~f:iterator pcap
  | None      -> ()

let command =
  Command.basic
  ~summary:"Parse PCAP files"
  ~readme:(fun () -> "More detailed information")
  spec
  (fun filename () -> operation filename)

let () =
  Command.run ~version:"1.0" ~build_info:"RWO" command
