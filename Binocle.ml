open Batteries

let version = "v0.10.0"

(* For convenience, we'd rather use top level variables for metrics, which will
 * be implicitly initialized during modules initialization. The problem is that
 * at this time we don't know yet if failing to read/save metric saved values
 * should be ignored (my_program --version) or crash (my_program --run-service)
 * therefore Binocle will just work on a "best-effort" basis and merely set
 * this variable whenever it failed to read/write a file, so that the program
 * can decide at startup (and later) whether to start (or continue) the service
 * or not.
 *
 * In the future we could have two classes of metrics, with one that always
 * ignore errors for convenience only metrics. *)
let last_error = ref None

type measure =
  | MFloat of float
  | MInt of int
  | MFloatRange of (float * float * float) (* min, value, max *)
  | MIntRange of (int * int * int) (* min, value, max *)
  | MString of string
  | MHistogram of (float * float * int) array

type kind = Counter | Gauge | Histogram

type label = string * string
  [@@ppp PPP_OCaml]

let print_label oc (l, v) = Printf.fprintf oc "%s=%S" l v

let print_labels oc labels =
  List.print ~first:"{" ~last:"}" ~sep:"," print_label oc labels

type metric =
  { name : string ;
    kind : kind ;
    labels : label list ;
    measure : measure }

let all_measures : (string, string * (unit -> metric list)) Hashtbl.t =
  Hashtbl.create 71

module Priv_ =
struct
  (*$< Priv_*)
  let round_to_int f =
    let frac, _ = modf f
    and n = int_of_float f in
    if abs_float frac <= 0.5 then n else if f < 0. then n-1 else n+1

  (*$= round_to_int & ~printer:string_of_int
    42 (round_to_int 42.1)
    42 (round_to_int 41.9)
    ~-42 (round_to_int ~-.42.1)
    ~-42 (round_to_int ~-.41.9)
   *)

  let wrap v = if v >= 0 then v else v - min_int

  (*$= wrap & ~printer:string_of_int
    9 (wrap (3 + 6))
    0 (wrap (max_int - 5 + 6))
    2 (wrap (max_int - 5 + 8))
   *)

  let now_us now =
    let now_s = int_of_float now in
    let now_us = (now -. float_of_int now_s) *. 1_000_000.0 |> round_to_int in
    Printf.sprintf "%d%06d" now_s now_us

  (*$= now_us & ~printer:BatPervasives.identity
    "1395066363000" (now_us 1395066.363)
    "1395066363042" (now_us 1395066.363042)
    "1395066000042" (now_us 1395066.000042)
   *)

  let print_val to_string name labels now oc x =
    Printf.fprintf oc "%s{" name ;
    List.iteri (fun i label ->
        Printf.fprintf oc "%s%a" (if i > 0 then "," else "") print_label label
      ) labels ;
    Printf.fprintf oc "} %s %s\n" (to_string x) now

  (* Gauges are made of three values (min, value, max): *)
  let print_gauge to_string name labels now oc (mi, x, ma) =
    print_val to_string name labels now oc x ;
    print_val to_string (name ^"_min") labels now oc mi ;
    print_val to_string (name ^"_max") labels now oc ma

  let print_option printer to_string name labels now oc xr =
    match !xr with
    | Some x -> printer to_string name labels now oc x
    | None -> ()

  let print_val_option to_string = print_option print_val to_string
  let print_gauge_option to_string = print_option print_gauge to_string

  let with_open_fd fname ro f =
    let open Legacy.Unix in
    let flags = if ro then [O_RDONLY] else [O_WRONLY;O_CREAT;O_TRUNC] in
    let fd = openfile fname flags 0o644 in
    finally (fun () -> close fd)
      f fd

  let with_locked_fd fname ro f =
    let open Legacy.Unix in
    with_open_fd fname ro (fun fd ->
      let op = if ro then F_RLOCK else F_LOCK in
      lockf fd op 0 ;
      finally
        (fun () ->
          ignore_exceptions (lseek fd 0) SEEK_SET ;
          lockf fd F_ULOCK 0)
        f fd)
  (*$>*)
end

module Labeled (T : sig type t val t_ppp_ocaml : t PPP.t end) =
struct
  type per_labels = (label list, T.t) Hashtbl.t
    [@@ppp PPP_OCaml]

  type t =
    { name : string ;
      help : string ;
      save_file : string option ;
      mutable per_labels : per_labels }

  let of_file fname =
    Priv_.with_locked_fd fname true (fun fd ->
      let ic = Legacy.Unix.in_channel_of_descr fd in
      PPP.of_in_channel_exc per_labels_ppp_ocaml ic)

  let to_file fname v =
    try
      Priv_.with_locked_fd fname false (fun fd ->
        let oc = Legacy.Unix.out_channel_of_descr fd in
        ignore_exceptions (fun () ->
          PPP.to_out_channel per_labels_ppp_ocaml oc v ;
          Legacy.flush oc) ())
    with e -> last_error := Some e

  let labeled_observation make_measure observe t ?(labels=[]) v =
    let labels = List.fast_sort Pervasives.compare labels in
    (* Maybe another program changed the saved value. If so, reload it
     * before proceeding: *)
    Option.may (fun fname ->
      (* TODO: only if the file changed: *)
      try t.per_labels <- of_file fname
      with e ->
        last_error := Some e ;
        Printf.eprintf "Could not read %s: %s\n%s.\nIgnoring...\n"
          fname
          (Printexc.to_string e)
          (Printexc.get_backtrace ())
    ) t.save_file ;
    (match Hashtbl.find t.per_labels labels with
    | exception Not_found ->
      let m = make_measure () in
      observe m v ;
      Hashtbl.add t.per_labels labels m
    | prev ->
      observe prev v) ;
    Option.may (fun fname ->
      to_file fname t.per_labels
    ) t.save_file

  let get ?(labels=[]) t =
    Hashtbl.find t.per_labels labels

  let export_all export_measure t =
    Hashtbl.fold (fun labels m lst ->
      export_measure m |>
      List.fold_left (fun lst (kind, measure) ->
        { name = t.name ; labels ; kind ; measure } :: lst
      ) lst
    ) t.per_labels []

  (* If [save_dir] is set then current value of the counter will be saved
   * in this file at exit, and loaded from it at creation: *)
  let make export_measure ?save_dir name help =
    let save_file = Option.map (fun d -> d ^"/"^ name) save_dir in
    assert (not (Hashtbl.mem all_measures name)) ;
    let make_new () =
      { name ; help ; per_labels = Hashtbl.create 17 ; save_file } in
    let t =
      match save_file with
      | None -> make_new ()
      | Some fname ->
          let t =
            try { name ; help ; per_labels = of_file fname ; save_file }
            with e ->
              last_error := Some e ;
              let t = make_new () in
              (* Make sure saving works as early as possible: *)
              to_file fname t.per_labels ;
              t in
          at_exit (fun () -> to_file fname t.per_labels) ;
          t
    in
    Hashtbl.add all_measures name
      (help, (fun () -> export_all export_measure t)) ;
    t

  (* Kind-of following Prometheus style *)
  let print print_measure now oc t =
    let now_us = Priv_.now_us now in
    Printf.fprintf oc "# HELP %s %s\n" t.name t.help ;
    Hashtbl.iter (fun labels m ->
        print_measure t.name labels now_us oc m
      ) t.per_labels ;
    Printf.fprintf oc "\n"
end

(* An int counter is the simplest of all measures.
 * Int counters are unsigned (wrap around back to 0). *)
module IntCounter =
struct
  module L = Labeled (struct type t = int ref [@@ppp PPP_OCaml] end)

  (* Build an int counter *)
  let make =
    let export m = [ Counter, MInt !m ] in
    L.make export

  (* Add an observation to this measure *)
  let add t =
    let observe m v = m := Priv_.wrap (!m + v)
    and make () = ref 0 in
    L.labeled_observation make observe t

  let inc ?labels t = add ?labels t 1
  let dec ?labels t = add ?labels t ~-1

  (* Directly set the value of the measure *)
  let set t =
    let observe m v = m := v
    and make () = ref 0 in
    L.labeled_observation make observe t

  (* Retrieve the current value (notwithstanding other programs updating the
   * same persisted metric) *)
  let get ?labels t =
    try !(L.get ?labels t) with Not_found -> 0

  (* Print the value kind-of Prometheus way *)
  let print now oc t =
    L.print (Priv_.print_val (fun m -> string_of_int !m)) now oc t
end

(* Float counters do not wrap around. Floats make actually very poor counters
 * as their accuracy decrease as the value raises. Think twice before using
 * them! *)
module FloatCounter =
struct
  module L = Labeled (struct type t = float ref [@@ppp PPP_OCaml] end)

  let make =
    let export m = [ Counter, MFloat !m ] in
    L.make export

  let add t =
    let observe m v = m := !m +. v
    and make () = ref 0. in
    L.labeled_observation make observe t

  let set t =
    let observe m v = m := v
    and make () = ref 0. in
    L.labeled_observation make observe t

  let get ?labels t =
    try !(L.get ?labels t) with Not_found -> 0.

  let print now oc t =
    L.print (Priv_.print_val (fun m -> string_of_float !m)) now oc t
end

(* TODO: A special kind of gauge that keep only the min/max? *)

module IntGauge =
struct
  module L =
    Labeled (struct
      type t = (int * int * int) option ref [@@ppp PPP_OCaml]
    end)

  let make =
    let export m =
      match !m with
      | None -> []
      | Some range -> [ Gauge, MIntRange range ] in
    L.make export

  let set t =
    let observe m v =
      m :=
        match !m with
        | None -> Some (v, v, v)
        | Some (mi, _, ma) -> Some (Int.min mi v, v, Int.max ma v)
    and make () = ref None in
    L.labeled_observation make observe t

  let get ?labels t =
    try !(L.get ?labels t) with Not_found -> None

  let print now oc t =
    L.print (Priv_.print_gauge_option string_of_int) now oc t
end

module FloatGauge =
struct
  module L =
    Labeled (struct
      type t = (float * float * float) option ref [@@ppp PPP_OCaml]
    end)

  let make =
    let export m =
      match !m with
      | None -> []
      | Some range -> [ Gauge, MFloatRange range ] in
    L.make export

  let set t =
    let observe m v =
      m :=
        match !m with
        | None -> Some (v, v, v)
        | Some (mi, _, ma) -> Some (Float.min mi v, v, Float.max ma v)
    and make () = ref None in
    L.labeled_observation make observe t

  let get ?labels t =
    try !(L.get ?labels t) with Not_found -> None

  let print now oc t =
    L.print (Priv_.print_gauge_option string_of_float) now oc t
end

module StringValue =
struct
  (* No min/max for strings *)
  module L = Labeled (struct type t = string option ref [@@ppp PPP_OCaml] end)

  let make =
    let export m =
      match !m with
      | None -> []
      | Some s -> [ Gauge, MString s ] in
    L.make export

  let set t =
    let observe m v = m := Some v
    and make () = ref None in
    L.labeled_observation make observe t

  let get ?labels t =
    try !(L.get ?labels t) with Not_found -> None

  let print now oc t =
    L.print (Priv_.print_val_option identity) now oc t
end

module StringConst =
struct
  module L = Labeled (struct type t = string [@@ppp PPP_OCaml] end)

  let make s name help =
    let export m = [ Gauge, MString m ] in
    s, L.make export name help

  let get ?labels t = L.get ?labels t

  let print now oc t =
    L.print (Priv_.print_val identity) now oc t
end

module Timestamp = FloatGauge

module Perf =
struct
  type t =
    { mutable count : int ; mutable user : float ; mutable system : float }
    [@@ppp PPP_OCaml]

  type perf_started = Unix.process_times option
  type perf_stopped = t option

  (* First some helpers to measure (statistically) CPU consumption as a
   * type [perf]: *)

  (* Might start to time, or do nothing: *)
  let start () =
    if Random.float 1. <= 0.01 then Some (Unix.times ())
    else None

  let stop_ start =
    let stop = Unix.times () in
    { user = Unix.(stop.tms_utime -. start.tms_utime) ;
      system = Unix.(stop.tms_stime -. start.tms_stime) ;
      count = 1 },
    stop

  let stop started =
    Option.map (fun start ->
      stop_ start |> fst
    ) started

  let transfer started =
    Option.map (fun start ->
      stop_ start
    ) started

  (* Which then can be added to some counter: *)

  module L = Labeled (struct
    type obs = t
    let obs_ppp_ocaml = t_ppp_ocaml
    type t = obs [@@ppp PPP_OCaml]
  end)

  let make =
    let export m =
      [ Counter, MInt m.count ;
        Counter, MFloat m.user ;
        Counter, MFloat m.system ] in
    L.make export

  let add t ?labels stopped =
    let observe m stopped =
      (* Detect cases where [stop] has not been called: *)
      assert (stopped.count > 0) ;
      m.count <- m.count + stopped.count ;
      m.user <- m.user +. stopped.user ;
      m.system <- m.system +. stopped.system
    and make () = { count = 0 ; user = 0. ; system = 0. } in
    Option.may (L.labeled_observation make observe t ?labels) stopped

  let add_and_transfer t ?labels started =
    Option.bind (transfer started) (fun (stopped, started) ->
      add t ?labels (Some stopped) ;
      Some started)

  let get ?labels t =
    try Some (L.get ?labels t) with Not_found -> None

  let print now oc t =
    let print_perf name labels now oc p =
      Priv_.print_val string_of_int (name ^".count") labels now oc p.count ;
      Priv_.print_val string_of_float (name ^".user") labels now oc p.user ;
      Priv_.print_val string_of_float (name ^".system") labels now oc p.system
    in
    L.print print_perf now oc t
end

(* Return a single measure composed of several buckets *)
module Histogram =
struct
  type histo = {
    (* Hash from bucket starting value (float) to bucket ending value (float)
     * and number of measurement in this bucket (int): *)
    counts : (float, float * int) Hashtbl.t ;
    mutable sum : float (* total sum of all observations *) }
    [@@ppp PPP_OCaml]

  module L = Labeled (struct type t = histo [@@ppp PPP_OCaml] end)

  let make ?save_dir name help bucket_of_value =
    let export m =
      let a = Array.make (Hashtbl.length m.counts) (0., 0., 0) in
      Hashtbl.fold (fun mi (ma, c) i ->
          a.(i) <- (mi, ma, c) ;
          i+1
        ) m.counts 0 |> ignore ;
      Array.sort (fun (mi1, _, _) (mi2, _, _) ->
          compare mi1 mi2
        ) a ;
      if Array.length a > 0 then
        [ Histogram, MHistogram a ]
      else [] in
    bucket_of_value, L.make ?save_dir export name help

  let add (bucket_of_value, t) =
    let observe m v =
      let mi, ma = bucket_of_value v in
      m.sum <- m.sum +. v ;
      match Hashtbl.find m.counts mi with
        | exception Not_found ->
          Hashtbl.add m.counts mi (ma, 1)
        | _, c ->
          Hashtbl.replace m.counts mi (ma, c+1)
    and make () =
      { counts = Hashtbl.create 11 ; sum = 0. } in
    L.labeled_observation make observe t

  let print now oc (_, t) =
    let print_measure name labels now_us oc histo =
      let count =
        let name_bucket = name ^"_bucket" in
        let buckets =
          Hashtbl.fold (fun _k v lst -> v :: lst) histo.counts [] |>
          List.fast_sort (fun (ma1, _) (ma2, _) ->
            compare (ma1:float) (ma2:float)) in
        List.fold_left (fun count (ma, c) ->
            let count = count + c in
            let labels = ("le", string_of_float ma) :: labels in
            Priv_.print_val string_of_int name_bucket labels now_us oc count ;
            count
          ) 0 buckets in
      Priv_.print_val string_of_float (name ^"_sum") labels now_us oc histo.sum ;
      Priv_.print_val string_of_int (name ^"_count") labels now_us oc count in
    L.print print_measure now oc t

  (* bucket_of_value ideas: *)
  let linear_buckets bucket_size v =
    let mi = floor (v /. bucket_size) *. bucket_size in
    mi, mi +. bucket_size

  let powers_of_two v =
    let ln2 = 0.69314718055994530942 in
    let log2 x = log x /. ln2 in
    let mf = log2 v in
    let flo = floor mf and cei = ceil mf in
    let cei = if cei = flo then cei +. 1. else cei in
    2. ** flo, 2. ** cei
end

(*$R
  let query_count =
    IntCounter.make "test_counter" "Demo of a labelled counter" in
  IntCounter.add query_count ~labels:["context","test"; "status","ok"] 40 ;
  IntCounter.add query_count ~labels:["context","test"; "status","ok"] 2 ;
  IntCounter.add query_count ~labels:["context","test"; "status","nok"] 7 ;
  let s = BatIO.to_string (IntCounter.print 1500136019.012674) query_count in
  assert_equal ~printer:BatPervasives.identity
    "# HELP test_counter Demo of a labelled counter\n\
     test_counter{context=\"test\",status=\"ok\"} 42 1500136019012674\n\
     test_counter{context=\"test\",status=\"nok\"} 7 1500136019012674\n\n" s ;

  let response_time =
    Histogram.make "test_histo" "Demo of a labelled histogram" (Histogram.linear_buckets 1.) in
  Histogram.add response_time ~labels:["context","test"; "status","ok"] 0.1 ;
  Histogram.add response_time ~labels:["context","test"; "status","ok"] 0.12 ;
  Histogram.add response_time ~labels:["context","test"; "status","ok"] 1.5 ;
  let s = BatIO.to_string (Histogram.print 1500136019.000001) response_time in
  assert_equal ~printer:BatPervasives.identity
    "# HELP test_histo Demo of a labelled histogram\n\
     test_histo_bucket{le=\"1.\",context=\"test\",status=\"ok\"} 2 1500136019000001\n\
     test_histo_bucket{le=\"2.\",context=\"test\",status=\"ok\"} 3 1500136019000001\n\
     test_histo_sum{context=\"test\",status=\"ok\"} 1.72 1500136019000001\n\
     test_histo_count{context=\"test\",status=\"ok\"} 3 1500136019000001\n\n" s ;

  let ram_usage =
    IntGauge.make "test_gauge" "Demo of a labelled gauge" in
  IntGauge.set ram_usage ~labels:["context","test"] 42 ;
  let s = BatIO.to_string (IntGauge.print 1500136019.000001) ram_usage in
  assert_equal ~printer:BatPervasives.identity
    "# HELP test_gauge Demo of a labelled gauge\n\
     test_gauge{context=\"test\"} 42 1500136019000001\n\
     test_gauge_min{context=\"test\"} 42 1500136019000001\n\
     test_gauge_max{context=\"test\"} 42 1500136019000001\n\n" s
 *)

(*
 * Utility functions to print (on console) all known metrics:
 *)

let window_width =
  try Sys.getenv "COLUMNS" |> int_of_string
  with _ ->
    (try
      let _, s = Unix.run_and_read "stty size 2>/dev/null" in
      Scanf.sscanf s "%d %d" (fun _h w -> w)
    with _ -> 80)

let colored ansi =
  Printf.sprintf "\027[%sm%s\027[0m" ansi

let grey = colored "1;30"
let red = colored "1;31"
let green = colored "1;32"
let yellow = colored "1;33"
let blue = colored "1;34"
let magenta = colored "1;35"
let cyan = colored "1;36"
let white = colored "1;37"

let make_bar n max_n max_width =
  let l = (n * max_width) / max_n in
  String.make l '='

let display_measure oc = function
  | MFloat v ->
      Printf.fprintf oc " %s" (blue (string_of_float v))
  | MInt v ->
      Printf.fprintf oc " %s" (blue (string_of_int v))
  | MFloatRange (mi, v, ma) ->
      Printf.fprintf oc " %s.. %s ..%s"
        (grey (string_of_float mi))
        (blue (string_of_float v))
        (grey (string_of_float ma))
  | MIntRange (mi, v, ma) ->
      Printf.fprintf oc " %s.. %s ..%s"
        (grey (string_of_int mi))
        (blue (string_of_int v))
        (grey (string_of_int ma))
  | MString v ->
      Printf.fprintf oc " %s" (blue v)
  | MHistogram v ->
      let bucket_cmp (v1, _, _) (v2, _, _) = Float.compare v1 v2 in
      Array.fast_sort bucket_cmp v ;
      let v = Array.map (fun (mi, ma, n) ->
                Printf.sprintf "%f..%f" mi ma,
                Printf.sprintf "%d" n,
                n) v in
      let max_intv, max_count, max_n =
        Array.fold_left (fun (ma1, ma2, ma_n) (s1, s2, n) ->
          assert (n >= 0) ;
          max ma1 (String.length s1),
          max ma2 (String.length s2),
          max ma_n n) (0, 0, 0) v in
      let max_bar_len = window_width - max_intv - max_count - 8 in
      Array.print ~first:"\n" ~last:"" ~sep:"\n" (fun oc (intv, count, n) ->
        let bar = make_bar n max_n max_bar_len in
        Printf.fprintf oc "    %*s: %s %s" max_intv intv bar (blue count)) oc v

let display_metric oc metric =
  List.print ~first:"  " ~last:" ->" ~sep:", " (fun oc (n, v) ->
    Printf.fprintf oc "%s: %s" (green n) (yellow v)) oc metric.labels ;
  display_measure oc metric.measure

let display_console () =
  Hashtbl.iter (fun name (help, export) ->
    Printf.printf "%s (%s)\n" (white help) (grey name) ;
    match export () with
    | [] -> Printf.printf "  no information\n\n"
    | metrics ->
        List.print ~first:"" ~last:"\n\n" ~sep:"\n" display_metric
          stdout metrics
  ) all_measures ;
  Printf.printf "%!"
