open Batteries


type measure = MFloat of float
             | MInt of int
             | MString of string
             | MHistogram of (float * float * int) array

type kind = Counter | Gauge | Histogram

type label = string * string
let print_label oc (l, v) = Printf.fprintf oc "%s=%S" l v
let print_labels oc labels =
  List.print ~first:"{" ~last:"}" ~sep:"," print_label oc labels

type metric =
  { name : string ;
    kind : kind ;
    labels : label list ;
    measure : measure }

let all_measures : (string, (unit -> metric list)) Hashtbl.t =
  Hashtbl.create 71

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

let print_val_option to_string name labels now oc xr =
  match !xr with
  | Some x -> print_val to_string name labels now oc x
  | None -> ()

module Labeled (T : sig type t end) =
struct
  type t =
    { name : string ;
      help : string ;
      per_labels : (label list, T.t) Hashtbl.t }

  let labeled_observation make_measure observe t labels v =
    let labels = List.fast_sort Pervasives.compare labels in
    match Hashtbl.find t.per_labels labels with
    | exception Not_found ->
      let m = make_measure () in
      observe m v ;
      Hashtbl.add t.per_labels labels m
    | prev ->
      observe prev v

  let export_all export_measure t =
    Hashtbl.fold (fun labels m lst ->
        match export_measure m with
        | None -> lst
        | Some (kind, measure) ->
          { name = t.name ; labels ; kind ; measure } :: lst
      ) t.per_labels []

  let make export_measure name help =
    let t =
      { name ; help ; per_labels = Hashtbl.create 17 } in
    assert (not (Hashtbl.mem all_measures name)) ;
    Hashtbl.add all_measures name (fun () -> export_all export_measure t) ;
    t

  (* Kind-of following Prometheus style *)
  let print print_measure now oc t =
    let now_us = now_us now in
    Printf.fprintf oc "# HELP %s %s\n" t.name t.help ;
    Hashtbl.iter (fun labels m ->
        print_measure t.name labels now_us oc m
      ) t.per_labels ;
    Printf.fprintf oc "\n"
end

(* An int counter is the simplest of all measures *)
module IntCounter =
struct
  module L = Labeled (struct type t = int ref end)

  (* Build an int counter *)
  let make =
    let export m = Some (Counter, MInt !m) in
    L.make export

  (* Add an observation to this measure *)
  let add t =
    let observe m v = m := !m + v
    and make () = ref 0 in
    L.labeled_observation make observe t

  (* Directly set the value of the measure *)
  let set t =
    let observe m v = m := v
    and make () = ref 0 in
    L.labeled_observation make observe t

  (* Print the value kind-of Prometheus way *)
  let print now oc t =
    L.print (print_val (fun m -> string_of_int !m)) now oc t
end

module FloatCounter =
struct
  module L = Labeled (struct type t = float ref end)

  let make =
    let export m = Some (Counter, MFloat !m) in
    L.make export

  let add t =
    let observe m v = m := !m +. v
    and make () = ref 0. in
    L.labeled_observation make observe t

  let set t =
    let observe m v = m := v
    and make () = ref 0. in
    L.labeled_observation make observe t

  let print now oc t =
    L.print (print_val (fun m -> string_of_float !m)) now oc t
end

(* TODO: A special kind of gauge that keep only the min/max? *)

module IntGauge =
struct
  module L = Labeled (struct type t = int option ref end)

  let make =
    let export m = Option.map (fun m -> Gauge, MInt m) !m in
    L.make export

  let set t =
    let observe m v = m := Some v
    and make () = ref None in
    L.labeled_observation make observe t

  let print now oc t =
    L.print (print_val_option string_of_int) now oc t
end

module FloatGauge =
struct
  module L = Labeled (struct type t = float option ref end)

  let make =
    let export m = Option.map (fun m -> Gauge, MFloat m) !m in
    L.make export

  let set t =
    let observe m v = m := Some v
    and make () = ref None in
    L.labeled_observation make observe t

  let print now oc t =
    L.print (print_val_option string_of_float) now oc t
end

module StringValue =
struct
  module L = Labeled (struct type t = string option ref end)

  let make =
    let export m = Option.map (fun m -> Gauge, MString m) !m in
    L.make export

  let set t =
    let observe m v = m := Some v
    and make () = ref None in
    L.labeled_observation make observe t

  let print now oc t =
    L.print (print_val_option identity) now oc t
end

module StringConst =
struct
  module L = Labeled (struct type t = string end)

  let make s name help =
    let export m = Some (Gauge, MString m) in
    s, L.make export name help

  let print now oc t =
    L.print (print_val identity) now oc t
end

module Timestamp = FloatGauge

(* Return a single measure composed of several buckets *)
module Histogram =
struct
  type histo = {
    (* Hash from bucket starting value (float) to bucket ending value (float)
     * and number of measurement in this bucket (int): *)
    counts : (float, float * int) Hashtbl.t ;
    mutable sum : float (* total sum of all observations *) }

  module L = Labeled (struct type t = histo end)

  let make name help bucket_of_value =
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
        Some (Histogram, MHistogram a)
      else None in
    bucket_of_value, L.make export name help

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
            print_val string_of_int name_bucket labels now_us oc count ;
            count
          ) 0 buckets in
      print_val string_of_float (name ^"_sum") labels now_us oc histo.sum ;
      print_val string_of_int (name ^"_count") labels now_us oc count in
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
  IntCounter.add query_count ["context","test"; "status","ok"] 40 ;
  IntCounter.add query_count ["context","test"; "status","ok"] 2 ;
  IntCounter.add query_count ["context","test"; "status","nok"] 7 ;
  let s = BatIO.to_string (IntCounter.print 1500136019.012674) query_count in
  assert_equal ~printer:BatPervasives.identity
    "# HELP test_counter Demo of a labelled counter\n\
     test_counter{context=\"test\",status=\"ok\"} 42 1500136019012674\n\
     test_counter{context=\"test\",status=\"nok\"} 7 1500136019012674\n\n" s ;

  let response_time =
    Histogram.make "test_histo" "Demo of a labelled histogram" (Histogram.linear_buckets 1.) in
  Histogram.add response_time ["context","test"; "status","ok"] 0.1 ;
  Histogram.add response_time ["context","test"; "status","ok"] 0.12 ;
  Histogram.add response_time ["context","test"; "status","ok"] 1.5 ;
  let s = BatIO.to_string (Histogram.print 1500136019.000001) response_time in
  assert_equal ~printer:BatPervasives.identity
    "# HELP test_histo Demo of a labelled histogram\n\
     test_histo_bucket{le=\"1.\",context=\"test\",status=\"ok\"} 2 1500136019000001\n\
     test_histo_bucket{le=\"2.\",context=\"test\",status=\"ok\"} 3 1500136019000001\n\
     test_histo_sum{context=\"test\",status=\"ok\"} 1.72 1500136019000001\n\
     test_histo_count{context=\"test\",status=\"ok\"} 3 1500136019000001\n\n" s ;

  let ram_usage =
    IntGauge.make "test_gauge" "Demo of a labelled gauge" in
  IntGauge.set ram_usage ["context","test"] 42 ;
  let s = BatIO.to_string (IntGauge.print 1500136019.000001) ram_usage in
  assert_equal ~printer:BatPervasives.identity
    "# HELP test_gauge Demo of a labelled gauge\n\
     test_gauge{context=\"test\"} 42 1500136019000001\n\n" s
 *)
