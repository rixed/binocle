open Binocle

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

let prometheus ?(port=0) () =
  let open Unix in
  let sock = socket PF_INET SOCK_STREAM 0 in
  bind sock (ADDR_INET(inet_addr_any, port)) ;
  let port = match getsockname sock with
    | ADDR_INET (_, port) -> port
    | _ -> failwith "error while starting prometheus thread" in
  listen sock 1 ;
  ignore @@ Thread.create (fun () ->
    let rec loop () =
      let (srv_sock, _ ) = accept sock in
      let oc = BatIO.output_string () in
      print_prometheus_measure oc ;
      respond srv_sock (http_msg (BatIO.close_out oc)) ;
      close srv_sock ;
      loop ()
    in
    loop ()
  ) () ;
  port
