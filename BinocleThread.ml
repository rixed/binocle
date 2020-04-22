open Binocle

module ParserConfig = ParsersConfig.BlockList (ParsersConfig.FileReader)
module ParserConfigWithOffset = ParsersPositions.Offset (ParserConfig)
let make_stream fd = ParserConfig.make_stream fd, 0
module HttpParser = CodecHttp.MakeParser (ParserConfigWithOffset)

let respond fd msg =
  let s = CodecHttp.Msg.encode msg in
  Batteries.Legacy.Unix.write_substring fd s 0 (String.length s) |> ignore

let http_msg ?(code=200) ?(headers=[]) body =
  let headers =
    ("Access-Control-Allow-Origin", "*") ::
    ("Content-Length", String.length body |> string_of_int) ::
    ("Content-Type", "application/json") :: headers in
  CodecHttp.(Msg.{
    start_line = StartLine.Response StatusLine.{
      version = 1, 1 ;
      code ;
      msg = CodecHttp.text_of_code code } ;
    headers ; body })


let prometheus () =
  Thread.create (fun () ->
    let open Unix in
    let sock = socket PF_INET SOCK_STREAM 0 in
    bind sock (ADDR_INET(inet_addr_any, 0)) ;
    listen sock 1 ;
    let rec loop () =
      let (srv_sock, _ ) = accept sock in
      let parser_res =
        let open HttpParser in
        let stream = make_stream srv_sock in
        let p = P.((p >>: fun m -> Some m) ||| (eof >>: fun () -> None)) in
        (p [] None Parsers.no_error_correction stream |> P.to_result) in
      let oc = BatIO.output_string () in
      let now = Unix.gettimeofday () /. 1000. in
      Binocle.print_prometheus_measure oc ;
      respond srv_sock (http_msg (BatIO.close_out oc)) ;
      close srv_sock ;
      loop ()
    in
    loop ()
  ) () ;
