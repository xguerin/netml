open Core.Std
open PCap
open NetStack

let spec =
  let open Command.Spec in
  empty
  +> anon ("filename" %: file)

let rec skip_to_ipv4 ts len proto =
  let open NetStack.Ethernet in
  let open NetStack.Ethernet.Protocol in
  let open Option.Monad_infix in
  match proto with
  | Length l    ->  printf "len %d\n" l; None
  | IPv4 b      ->  printf "IPv4\n"; Some b
  | VLAN b      ->  printf "VLAN\n";
                    (NetStack.VLAN.decode (len, b)) >>= fun (len, vlan) -> skip_to_ipv4 ts len vlan.VLAN.protocol
  | Unsupported ->  printf "Unsupported\n"; None

let iterator ~ts ~data ~len =
  printf "%d %d " ts len;
  let open NetStack.Ethernet in
  let open NetStack.Ethernet.Protocol in
  let open NetStack.VLAN in
  let open Option.Monad_infix in
  ignore (
    NetStack.Ethernet.decode (len, data) >>= fun (len, eth) -> skip_to_ipv4 ts len eth.protocol
  )

let operation filename =
  match PCap.open_file filename with
  | Some pcap -> PCap.iter ~f:iterator pcap
  | None      -> ()

let command =
  Command.basic
  ~summary:"Parse PCAP files"
  ~readme:(fun () -> "More detailed information")
  spec
  (fun filename () -> operation filename)

let () =
  Command.run ~version:"1.0" ~build_info:"RWO" command
