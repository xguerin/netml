open Core.Std
open NetML.PCap
open NetML.Stack

let spec =
  let open Command.Spec in
  empty
  +> anon ("filename" %: file)

let process_ip ip =
  printf "%s\n" (NetML.Stack.IPv4.to_string ip);
  Some ip

let rec process_protocol proto =
  let open NetML.Stack.Ethernet.Protocol in
  match proto with
  | Length _    -> None
  | Unsupported -> None
  | IPv4 ip     -> process_ip ip
  | VLAN (_, v) -> printf "VLAN "; process_protocol v

let iterator ~ts ~data ~len =
  printf "%d %04d " ts len;
  let open NetML.Stack.Ethernet in
  let open NetML.Stack.Ethernet.Protocol in
  let open Option.Monad_infix in
  ignore (
    NetML.Stack.Ethernet.decode data >>= fun eth ->
    printf "%s " (NetML.Stack.Ethernet.to_string eth);
    process_protocol eth.protocol
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
