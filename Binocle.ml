open Batteries

let version = "v0.13.0"

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
  | MHistogram of (float * float * int) array * float

type kind = Counter | Gauge | Histogram

type label = string * string (* name, value *)
  [@@ppp PPP_OCaml]

let print_label oc (l, v) = Printf.fprintf oc "%s=%S" l v

let print_labels oc labels =
  List.print ~first:"{" ~last:"}" ~sep:"," print_label oc labels

(* As all the metrics defined by the `Labeled` functor do not have the same type
 * they are mapped to this `metric` type to process them all at once, in the
 * `display_console` function for example. *)
(* TODO: Add the timestamp of the last change *)
type metric =
  { kind : kind ;
    labels : label list ;
    measure : measure }

(* Contains all the metrics indexed by name.
 * Values are a pair with their help string and export function. *)
let all_measures : (string (* name *),
                    string (* help *) * (unit -> metric list)) Hashtbl.t =
  Hashtbl.create 71

module Priv_ =
struct
  (*$< Priv_*)
  let make_rate_limited delay =
    let last_done = ref 0. in
    fun now f ->
      if now -. !last_done >= delay then (
        last_done := now ;
        f ())

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

  let now_ms now =
    Printf.sprintf "%.0f" (now *. 1000.)

  (*$= now_ms & ~printer:BatPervasives.identity
    "1395066363" (now_ms 1395066.363)
    "1395066363" (now_ms 1395066.363042)
    "1395066000" (now_ms 1395066.000042)
   *)

  let string_forall f s =
    try
      for i = 0 to String.length s - 1 do
        if not (f (String.unsafe_get s i)) then raise Exit
      done ;
      true
    with Exit ->
      false

  (*$= string_forall & ~printer:string_of_bool
    true  (string_forall (fun c -> c >= '0' && c <= '9') "123123")
    false (string_forall (fun c -> c >= '0' && c <= '9') "1231x3")
  *)

  let with_open_fd fname ro f =
    let open Legacy.Unix in
    let flags = if ro then [O_RDONLY] else [O_RDWR;O_CREAT;O_CLOEXEC] in
    let fd = openfile fname flags 0o644 in
    finally (fun () -> close fd)
      f fd

  let with_locked_fd fname ro f =
    let open Legacy.Unix in
    with_open_fd fname ro (fun fd ->
      let op = if ro then F_RLOCK else F_LOCK in
      (BatUnix.restart_on_EINTR lockf fd op) 0 ;
      finally
        (fun () ->
          ignore_exceptions (lseek fd 0) SEEK_SET ;
          (BatUnix.restart_on_EINTR lockf fd F_ULOCK) 0)
        f fd)

  (* Returns the first few bytes of a file, or "ERROR" *)
  let beginning_of_file ?(max=20) fname =
    let res = Bytes.create max in
    try
      with_open_fd fname true (fun fd ->
        let rec loop o =
          if o >= max then res else
          let r = Unix.(restart_on_EINTR read fd res o) (max - o) in
          if r = 0 then Bytes.sub res 0 o else
          loop (o + r) in
        loop 0 |> Bytes.to_string)
    with e ->
      last_error := Some e ;
      "ERROR"

  let mtime_of_fd fname =
    Unix.((restart_on_EINTR fstat fname).st_mtime)
  (*$>*)
end

(* From a base type [t], creates the basic functions to associate measures
 * of that type and labels. This module is then included in all actual
 * instrumentation metric types. *)
module Labeled (T : sig type t val t_ppp_ocaml : t PPP.t end) =
struct
  (* Metrics are identified by a label set *)
  type per_labels = (label list, T.t) Hashtbl.t
    [@@ppp PPP_OCaml]

  type t =
    { name : string ;
      save_file : string option ;
      mutable per_labels : per_labels ;
      mutable last_read_file : float }

  let of_file fd =
    let ic = Legacy.Unix.in_channel_of_descr fd in
    PPP.of_in_channel_exc per_labels_ppp_ocaml ic

  let to_file fname v =
    try
      Priv_.with_locked_fd fname false (fun fd ->
        Unix.(restart_on_EINTR (ftruncate fd)) 0 ;
        let oc = Legacy.Unix.out_channel_of_descr fd in
        ignore_exceptions (fun () ->
          PPP.to_out_channel per_labels_ppp_ocaml oc v ;
          Legacy.flush oc) ())
    with e ->
      last_error := Some e

  let rate_limited_log = Priv_.make_rate_limited 5.

  (* Add an observation.
   * [make_measure] creates a new measure if [labels] are encountered
   *   for the first time.
   * [observe] modify an existing measure.
   * [t] is the metric we want to add the observation to.
   * [labels] identify the observed metric. *)
  let labeled_observation make_measure observe t ?(labels=[]) v =
    let labels = List.fast_sort Pervasives.compare labels in
    let update () =
      match Hashtbl.find t.per_labels labels with
      | exception Not_found ->
        let m = make_measure () in
        observe m v ;
        Hashtbl.add t.per_labels labels m
      | prev ->
        observe prev v in
    match t.save_file with
    | None ->
        update ()
    | Some fname ->
        (* Maybe another program changed the saved value. If so, reload it
         * before proceeding: *)
        let now = Unix.gettimeofday () in
        try
          Priv_.with_locked_fd fname false (fun fd ->
            let last_changed = Priv_.mtime_of_fd fd in
            if last_changed >= t.last_read_file then (
              t.last_read_file <- now ;
              try t.per_labels <- of_file fd
              with e ->
                last_error := Some e ;
                rate_limited_log now (fun () ->
                  Printf.eprintf "Could not read %s (%S): %s\n%s.\nIgnoring...\n%!"
                    fname
                    (Priv_.beginning_of_file fname)
                    (Printexc.to_string e)
                    (Printexc.get_backtrace ()))) ;
            update () ;
            to_file fname t.per_labels)
        with e ->
          last_error := Some e ;
          rate_limited_log now (fun () ->
            Printf.eprintf "Could not open %s: %s\n%s.\nIgnoring...\n%!"
              fname
              (Printexc.to_string e)
              (Printexc.get_backtrace ())) ;
          update () (* Since to_file does not raise *)

  let get ?(labels=[]) t =
    Hashtbl.find t.per_labels labels

  (* This function is to export measure to a general type *)
  let export_all export_measure t =
    Hashtbl.fold (fun labels m lst ->
      export_measure m |>
      List.fold_left (fun lst (kind, measure) ->
        { labels ; kind ; measure } :: lst
      ) lst
    ) t.per_labels []

  (* Makes a new metric.
   * [export_measure] is a function that convert a single measure to the more
   * general type [measure].
   * If [save_dir] is set then value of the measure will be read from and
   * written to this location after every new observation *)
  let make export_measure ?save_dir name help =
    let save_file = Option.map (fun d -> d ^"/"^ name) save_dir in
    if Hashtbl.mem all_measures name then
      failwith ("Metric "^ name ^" defined several times") ;
    let make_new () =
      { name ; per_labels = Hashtbl.create 17 ; save_file ;
        last_read_file = 0. } in
    let t =
      match save_file with
      | None -> make_new ()
      | Some fname ->
          let t =
            try
              Priv_.with_locked_fd fname true (fun fd ->
                { name ; per_labels = of_file fd ; save_file ;
                  last_read_file = 0. })
              (* Little need to test writing if reading worked *)
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

  let inc ?labels t = add ?labels t 1.
  let dec ?labels t = add ?labels t ~-.1.

  let set t =
    let observe m v = m := v
    and make () = ref 0. in
    L.labeled_observation make observe t

  let get ?labels t =
    try !(L.get ?labels t) with Not_found -> 0.
end

(* TODO: A special kind of gauge that keeps only the min/max? *)

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

  let add t =
    let observe m v =
      (* Assume starting at 0 when using add/sub *)
      let mi, p, ma = !m |? (0, 0, 0) in
      let p = p + v in
      m := Some (Int.min mi p, p, Int.max ma p)
    and make () = ref None in
    L.labeled_observation make observe t

  let inc ?labels t = add ?labels t 1
  let dec ?labels t = add ?labels t ~-1
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

  let add t =
    let observe m v =
      (* Assume starting at 0 when using add/sub *)
      let mi, p, ma = !m |? (0., 0., 0.) in
      let p = p +. v in
      m := Some (Float.min mi p, p, Float.max ma p)
    and make () = ref None in
    L.labeled_observation make observe t

  let inc ?labels t = add ?labels t 1.
  let dec ?labels t = add ?labels t ~-.1.
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
end

module StringConst =
struct
  module L = Labeled (struct type t = string [@@ppp PPP_OCaml] end)

  let make s name help =
    let export m = [ Gauge, MString m ] in
    s, L.make export name help

  let get ?labels t = L.get ?labels t
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
  let start ?(rate=0.01) ()=
    if Random.float 1. <= rate then Some (Unix.times ())
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

  (* FIXME: have instead 3 counters with different labels
  let print_perf name labels now oc p =
    Priv_.print_val string_of_int (name ^".count") labels now oc p.count ;
    Priv_.print_val string_of_float (name ^".user") labels now oc p.user ;
    Priv_.print_val string_of_float (name ^".system") labels now oc p.system *)
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
          Float.compare mi1 mi2
        ) a ;
      if Array.length a > 0 then
        [ Histogram, MHistogram (a, m.sum) ]
      else []
    in
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

(*
 * Utility functions to access the metrics:
 *)

let iter f =
  Hashtbl.iter (fun name (help, export) ->
    f name help export
  ) all_measures

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

let colored ?(colors=true) ansi =
  if colors then
    Printf.sprintf "\027[%sm%s\027[0m" ansi
  else
    Printf.sprintf "%s"

let grey ?colors = colored ?colors "1;30"
let red ?colors = colored ?colors "1;31"
let green ?colors = colored ?colors "1;32"
let yellow ?colors = colored ?colors "1;33"
let blue ?colors = colored ?colors "1;34"
let magenta ?colors = colored ?colors "1;35"
let cyan ?colors = colored ?colors "1;36"
let white ?colors = colored ?colors "1;37"

let make_bar n max_n max_width =
  let l = (n * max_width) / max_n in
  String.make l '='

let display_measure ?colors oc = function
  | MFloat v ->
      Printf.fprintf oc " %s" (blue ?colors (string_of_float v))
  | MInt v ->
      Printf.fprintf oc " %s" (blue ?colors (string_of_int v))
  | MFloatRange (mi, v, ma) ->
      Printf.fprintf oc " %s.. %s ..%s"
        (grey ?colors (string_of_float mi))
        (blue ?colors (string_of_float v))
        (grey ?colors (string_of_float ma))
  | MIntRange (mi, v, ma) ->
      Printf.fprintf oc " %s.. %s ..%s"
        (grey ?colors (string_of_int mi))
        (blue ?colors (string_of_int v))
        (grey ?colors (string_of_int ma))
  | MString v ->
      Printf.fprintf oc " %s" (blue ?colors v)
  | MHistogram (v, _) ->
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
        Printf.fprintf oc "    %*s: %s %s" max_intv intv bar (blue ?colors count)) oc v

let display_metric ?colors oc metric =
  List.print ~first:"  " ~last:" ->" ~sep:", " (fun oc (n, v) ->
    Printf.fprintf oc "%s: %s" (green ?colors n) (yellow ?colors v)) oc metric.labels ;
  display_measure ?colors oc metric.measure

let display_console ?colors () =
  iter (fun name help export ->
    Printf.printf "%s (%s)\n" (white ?colors help) (grey ?colors name) ;
    match export () with
    | [] -> Printf.printf "  no information\n\n"
    | metrics ->
        List.print ~first:"" ~last:"\n\n" ~sep:"\n" (display_metric ?colors)
          stdout metrics
  ) ;
  Printf.printf "%!"

(*
 * Expose the measures in Prometheus format
 *)
module Prometheus =
struct
  (*$< Prometheus *)

  let print_val to_string name labels now oc x =
    Printf.fprintf oc "%s{" name ;
    List.iteri (fun i label ->
        Printf.fprintf oc "%s%a"
          (if i > 0 then "," else "") print_label label
      ) labels ;
    Printf.fprintf oc "} %s %s\n" (to_string x) now

  let make_valid_name =
    let is_valid_first c =
      (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' || c = ':' in
    let is_valid c =
      is_valid_first c || (c >= '0' && c <= '9') in
    fun s ->
      let s =
        if s <> "" then s else "_" in
      let s =
        if is_valid_first s.[0] then s else "_" ^ s in
      let s =
        if Priv_.string_forall is_valid s then s else
        String.map (fun c -> if is_valid c then c else '_') s in
      s

  let expose_per_label now_ms name oc t =
    let p print_measure v =
      print_measure name t.labels now_ms oc v
    in
    match t.measure with
    | MInt v ->
        p (print_val string_of_int) v
    | MFloat v ->
        p (print_val string_of_float) v
    | MFloatRange (_, v, _) ->
        p (print_val string_of_float) v
    | MIntRange (_, v, _) ->
        p (print_val string_of_int) v
    | MString v ->
        p (print_val identity) v
    | MHistogram (v, s) ->
      let print_measure name labels now_ms oc v =
        let count =
          let name_bucket = name ^"_bucket" in
          Array.fold_left (fun count (_mi, ma, c) ->
              let count = count + c in
              let labels = ("le", string_of_float ma) :: labels in
              print_val string_of_int name_bucket labels now_ms oc count ;
              count
            ) 0 v in
        print_val string_of_float (name ^"_sum") labels now_ms oc s ;
        print_val string_of_int (name ^"_count") labels now_ms oc count
      in
      p print_measure v

  let expose_metrics now_ms namespace name help oc metrics =
    let name = namespace ^"_"^ name |> make_valid_name in
    (* It is expected all label sets will have the same type: *)
    let m1 = List.hd metrics in
    (* For ranges we fake to have 3 individual metrics: *)
    let mis, vs, mas =
      List.fold_left (fun (mis, vs, mas) t ->
        match t.measure with
        | MFloatRange (mi, v, ma) ->
            { t with measure = MFloat mi } :: mis,
            { t with measure = MFloat v } :: vs,
            { t with measure = MFloat ma } :: mas
        | MIntRange (mi, v, ma) ->
            { t with measure = MInt mi } :: mis,
            { t with measure = MInt v } :: vs,
            { t with measure = MInt ma } :: mas
        | _ ->
            mis, t :: vs, mas
      ) ([], [], []) metrics in
    let expose name help metrics =
      Printf.fprintf oc "# HELP %s %s\n" name help ;
      assert (metrics <> []) ;
      let typ =
        match m1.kind with
        | Counter -> "counter"
        | Gauge -> "gauge"
        | Histogram -> "histogram" in
      Printf.fprintf oc "# TYPE %s %s\n" name typ ;
      List.print ~first:"" ~last:"\n" ~sep:""
        (expose_per_label now_ms name) oc metrics
    in
    expose name help vs ;
    if mis <> [] then expose (name ^"_min") (help ^" (min)") mis ;
    if mas <> [] then expose (name ^"_max") (help ^" (max)") mas

  let expose namespace now oc measures =
    let now_ms = Priv_.now_ms now in
    Hashtbl.iter (fun name (help, export) ->
      match export () with
      | [] -> ()
      | metrics ->
          expose_metrics now_ms namespace name help oc metrics
    ) measures ;
    Printf.fprintf oc "%!"

  (*$R
    let query_count =
      IntCounter.make "counter" "Demo of a labelled counter" in
    IntCounter.add query_count ~labels:["context","test"; "status","ok"] 40 ;
    IntCounter.add query_count ~labels:["context","test"; "status","ok"] 2 ;
    IntCounter.add query_count ~labels:["context","test"; "status","nok"] 7 ;
    let to_string n =
      let now_ms = Priv_.now_ms 1500136019.012674 in
      let help, metrics = Hashtbl.find all_measures n in
      BatIO.to_string (expose_metrics now_ms "test" n help) (metrics ()) in
    assert_equal ~printer:BatPervasives.identity
      "# HELP test_counter Demo of a labelled counter\n\
       # TYPE test_counter counter\n\
       test_counter{context=\"test\",status=\"ok\"} 42 1500136019013\n\
       test_counter{context=\"test\",status=\"nok\"} 7 1500136019013\n\n"
      (to_string "counter") ;

    let response_time =
      Histogram.make "histo" "Demo of a labelled histogram" (Histogram.linear_buckets 1.) in
    Histogram.add response_time ~labels:["context","test"; "status","ok"] 0.1 ;
    Histogram.add response_time ~labels:["context","test"; "status","ok"] 0.12 ;
    Histogram.add response_time ~labels:["context","test"; "status","ok"] 1.5 ;
    assert_equal ~printer:BatPervasives.identity
      "# HELP test_histo Demo of a labelled histogram\n\
       # TYPE test_histo histogram\n\
       test_histo_bucket{le=\"1.\",context=\"test\",status=\"ok\"} 2 1500136019013\n\
       test_histo_bucket{le=\"2.\",context=\"test\",status=\"ok\"} 3 1500136019013\n\
       test_histo_sum{context=\"test\",status=\"ok\"} 1.72 1500136019013\n\
       test_histo_count{context=\"test\",status=\"ok\"} 3 1500136019013\n\n"
      (to_string "histo") ;

    let ram_usage =
      IntGauge.make "gauge" "Demo of a labelled gauge" in
    IntGauge.set ram_usage ~labels:["context","test"] 42 ;
    assert_equal ~printer:BatPervasives.identity
      "# HELP test_gauge Demo of a labelled gauge\n\
       # TYPE test_gauge gauge\n\
       test_gauge{context=\"test\"} 42 1500136019013\n\
       \n\
       # HELP test_gauge_min Demo of a labelled gauge (min)\n\
       # TYPE test_gauge_min gauge\n\
       test_gauge_min{context=\"test\"} 42 1500136019013\n\
       \n\
       # HELP test_gauge_max Demo of a labelled gauge (max)\n\
       # TYPE test_gauge_max gauge\n\
       test_gauge_max{context=\"test\"} 42 1500136019013\n\n"
      (to_string "gauge")
   *)

  (*$>*)
end
