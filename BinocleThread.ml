open Batteries
open Binocle

let respond fd msg =
  let s = CodecHttp.Msg.encode msg in
  Unix.write_substring fd s 0 (String.length s) |> ignore

let http_msg ?(code=200) ?(headers=[]) body =
  let headers =
    ("Access-Control-Allow-Origin", "*") ::
    ("Content-Length", String.length body |> string_of_int) ::
    ("Content-Type", "text/plain") :: headers in
  CodecHttp.(Msg.{
    start_line = StartLine.Response StatusLine.{
      version = 1, 1 ;
      code ;
      msg = CodecHttp.text_of_code code } ;
    headers ; body })

(* [namespace] is the prefix prepended to each metric name (separated from
 * the metric name with an underscore). *)
let http_expose_prometheus ?(namespace="binocle") ?(port=0) () =
  Thread.create (forever (fun () ->
    let open Unix in
    let sock = socket PF_INET SOCK_STREAM 0 in
    bind sock (ADDR_INET (inet_addr_any, port)) ;
    let port = match getsockname sock with
      | ADDR_INET (_, port) -> port
      | _ -> assert false in
    listen sock 1 ;
    (try
      forever (fun () ->
        let (srv_sock, _ ) = restart_on_EINTR accept sock in
        let now = Unix.gettimeofday () in
        let s = BatIO.to_string (Prometheus.expose namespace now) all_measures in
        respond srv_sock (http_msg s) ;
        close srv_sock
      ) ()
    with e ->
      Printf.eprintf "While serving prometheus on port %d: %s"
        port (Printexc.to_string e))
  )) (),
  port
