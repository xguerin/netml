open Core.Std
open PCap
open NetStack

let spec =
  let open Command.Spec in
  empty
  +> anon ("filename" %: file)

let rec skip_to_ipv4 ts proto =
  let open NetStack.Ethernet in
  let open NetStack.Ethernet.Protocol in
  let open Option.Monad_infix in
  match proto with
  | Length l        ->  printf "len %d\n" l; None
  | IPv4 (len, bs)  ->  NetStack.IPv4.decode (len, bs)
  | VLAN (len, bs)  ->  printf "VLAN ";
                        NetStack.VLAN.decode (len, bs) >>= fun vlan ->
                        skip_to_ipv4 ts vlan.VLAN.protocol
  | Unsupported ->  printf "Unsupported\n"; None

let iterator ~ts ~data ~len =
  printf "%d %d " ts len;
  let open NetStack.Ethernet in
  let open NetStack.Ethernet.Protocol in
  let open NetStack.VLAN in
  let open Option.Monad_infix in
  ignore (
    NetStack.Ethernet.decode (len, data) >>= fun eth ->
    printf "%s " (NetStack.Ethernet.to_string eth);
    skip_to_ipv4 ts eth.protocol >>= fun ip ->
    printf "%s\n" (NetStack.IPv4.to_string ip);
    Some ip.IPv4.protocol
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
