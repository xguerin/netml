open Core.Std
open NetML

let spec =
  let open Command.Spec in
  empty
  +> flag "-t" no_arg ~doc:" show packet time stamp"
  +> flag "-s" no_arg ~doc:" show packet size"
  +> flag "-E" no_arg ~doc:" show Ethernet information"
  +> anon ("filename" %: file)

let rec iterate = function
  | Some (pcap) ->
    let open Option.Monad_infix in
    let open PCap in
    next_packet pcap >>= fun (pkt, rem) ->
    let pkt_hdr = Packet.header pkt in
    begin match pkt_hdr.Packet.Header.nettype with
      | Layer.II.Protocol.Ethernet -> None
      | _ -> None
    end
  | None -> None

let operation show_ts show_sz fn =
  let open Option.Monad_infix in
  ignore (
    PCap.open_file fn >>= fun pcap ->
    let json = PCap.Header.to_yojson (PCap.header pcap) in
    Printf.printf "%s\n" (Yojson.Safe.to_string json);
    iterate (Some (pcap))
  )

let command =
  Command.basic
  ~summary:"Parse PCAP files"
  ~readme:(fun () -> "More detailed information")
  spec
  (fun show_ts show_sz _ fn () -> operation show_ts show_sz fn)

let () =
  Command.run ~version:"1.0" ~build_info:"RWO" command
