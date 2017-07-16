open Batteries

type measure = MetricFloat of float
             | MetricInt of int
             | MetricString of string
             | MetricHistogram of (float * float * int) array [@@ppp PPP_OCaml]

let all_measures : (string, (unit -> measure option)) Hashtbl.t =
  Hashtbl.create 71

(* Add a measure into all_measures and return it. The name given is the name of
 * the family of measures exported *)
let registered name export t =
  assert (not (Hashtbl.mem all_measures name)) ;
  Hashtbl.add all_measures name (fun () -> export t) ;
  t

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

type label = string * string
let print_label oc (l, v) = Printf.fprintf oc "%s=%S" l v
let print_labels oc labels =
  List.print ~first:"{" ~last:"}" ~sep:"," print_label oc labels

let now_us now =
  let now_s = int_of_float now in
  let now_us = (now -. float_of_int now_s) *. 1_000_000.0 |> round_to_int in
  Printf.sprintf "%d%06d" now_s now_us

(*$= now_us & ~printer:BatPervasives.identity
  "1395066363000" (now_us 1395066.363)
  "1395066363042" (now_us 1395066.363042)
  "1395066000042" (now_us 1395066.000042)
 *)

let print_val name labels now oc s =
  Printf.fprintf oc "%s{" name ;
  List.iteri (fun i label ->
      Printf.fprintf oc "%s%a" (if i > 0 then "," else "") print_label label
    ) labels ;
  Printf.fprintf oc "} %s %s\n" s now

(* An int counter is the simplest of all measures *)
module IntCounter =
struct
  (* The type of an int counter *)
  type t = int ref
  (* The type of values we can observe (or add) *)
  type v = int
  (* Add an observation to this measure *)
  let add t v = t := !t + v
  (* Directly set the value of the measure *)
  let set t v = t := v
  (* Build the measure out of this set of observations *)
  let export t = Some (MetricInt !t)
  (* Build an int counter *)
  let make name = registered name export (ref 0)
  (* Print the value kind-of Prometheus way *)
  let print name labels now oc t =
    print_val name labels now oc (string_of_int !t)
end

module FloatCounter =
struct
  type t = float ref
  type v = float
  let add t v = t := !t +. v
  let set t v = t := v
  let export t = Some (MetricFloat !t)
  let make name = registered name export (ref 0.)
  let print name labels now oc t =
    print_val name labels now oc (string_of_float !t)
end

module IntGauge =
struct
  type t = int option ref
  type v = int
  let set t v = t := Some v
  let keep_max t v = match !t with (* I which there was a better way *)
    | None -> set t v
    | Some x when x < v -> set t v
    | _ -> ()
  let export t = match !t with
    | Some x -> Some (MetricInt x)
    | None -> None
  let make name = registered name export (ref None)
  let print name labels now oc t = match !t with
    | Some x -> print_val name labels now oc (string_of_int x)
    | None -> ()
end

module FloatGauge =
struct
  type t = float option ref
  type v = float
  let set t v = t := Some v
  let keep_max t v = match !t with
    | None -> set t v
    | Some x when x < v -> set t v
    | _ -> ()
  let export t = match !t with
    | Some x -> Some (MetricFloat x)
    | None -> None
  let make name = registered name export (ref None)
  let print name labels now oc t = match !t with
    | Some x -> print_val name labels now oc (string_of_float x)
    | None -> ()
end

module StringValue =
struct
  type t = string option ref
  type v = string
  let set t v = t := Some v
  let export t = match !t with
    | Some x -> Some (MetricString x)
    | None -> None
  let make name = registered name export (ref None)
  let print name labels now oc t =
    print_val name labels now oc t
end

module StringConst =
struct
  type t = string
  type v = string
  let export t = Some (MetricString t)
  let make name s = registered name export s
  let print name labels now oc t =
    print_val name labels now oc t
end

module Timestamp = FloatGauge

(* Return a single measure composed of several buckets *)
module Histogram =
struct
  type t = {
    (* Function returning the bucket starting and ending values for a given
     * measured value (so we are not limited to uniform scales): *)
    bucket_of_value : float -> (float * float) ;
    (* Hash from bucket starting value (float) to bucket ending value (float)
     * and number of measurement in this bucket (int): *)
    counts : (float, float * int) Hashtbl.t ;
    mutable sum : float (* total sum of all observations *) }

  (* type of values *)
  type v = float

  let add t v =
    let mi, ma = t.bucket_of_value v in
    t.sum <- t.sum +. v ;
    match Hashtbl.find t.counts mi with
      | exception Not_found ->
        Hashtbl.add t.counts mi (ma, 1)
      | _, c ->
        Hashtbl.replace t.counts mi (ma, c+1)

  (* Convert the hash into an array of buckets + count *)
  let export t =
    let a = Array.make (Hashtbl.length t.counts) (0., 0., 0) in
    Hashtbl.fold (fun mi (ma, c) i ->
        a.(i) <- (mi, ma, c) ;
        i+1
      ) t.counts 0 |> ignore ;
    Array.sort (fun (mi1, _, _) (mi2, _, _) ->
        compare mi1 mi2
      ) a ;
    Some (MetricHistogram a)

  let print name labels now oc t =
    let count =
      let name_bucket = name ^"_bucket" in
      let buckets =
        Hashtbl.fold (fun _k v lst -> v :: lst) t.counts [] |>
        List.fast_sort (fun (ma1, _) (ma2, _) ->
          compare (ma1:float) (ma2:float)) in
      List.fold_left (fun count (ma, c) ->
          let count = count + c in
          print_val name_bucket (("le", string_of_float ma)::labels) now oc (string_of_int count) ;
          count
        ) 0 buckets in
    print_val (name ^"_sum") labels now oc (string_of_float t.sum) ;
    print_val (name ^"_count") labels now oc (string_of_int count)

  let make bucket_of_value name =
    registered name export
      { bucket_of_value ; counts = Hashtbl.create 11 ; sum = 0. }

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

(* Allows to have arbitrary labels with a metric (by actually maintaining as
 * many metrics as we have encountered label values).  Labels as well as their
 * values are mere strings that bear no special meaning to Binocle. *)
module Labeled (Measure : sig
    type t
    type v
    val add : t -> v -> unit
    val print : string -> label list -> string -> 'a BatInnerIO.output -> t -> unit
  end) =
struct
  type t =
    { name : string ;
      help : string ;
      make_measure : string -> Measure.t ;
      per_labels : (label list, Measure.t) Hashtbl.t }

  let tot_name t labels =
    t.name ^"{"^
      (List.map (fun (l,v) -> Printf.sprintf "%s=%S" l v) labels |>
       String.concat ",")
    ^"}"

  let add t labels v =
    let labels = List.fast_sort Pervasives.compare labels in
    match Hashtbl.find t.per_labels labels with
    | exception Not_found ->
      let tot_name = tot_name t labels in
      let m = t.make_measure tot_name in
      Measure.add m v ;
      Hashtbl.add t.per_labels labels m
    | prev ->
      Measure.add prev v

  let make name help make_measure =
    (* Do not register at once but only when new labels are encountered *)
    { name ; help ; make_measure ; per_labels = Hashtbl.create 17 }

  (* Kind-of following Prometheus style *)
  let print now oc t =
    let now_us = now_us now in
    Printf.fprintf oc "# HELP %s %s\n" t.name t.help ;
    Hashtbl.iter (fun labels m ->
        Measure.print t.name labels now_us oc m
      ) t.per_labels ;
    Printf.fprintf oc "\n"
end

(*$inject
  module LabeledIntCounter = Labeled (IntCounter)
  module LabeledHisto = Labeled (Histogram)
 *)
(*$R
  let query_count =
    LabeledIntCounter.make "test_counter" "Demo of a labelled counter" IntCounter.make in
  LabeledIntCounter.add query_count ["context","test"; "status","ok"] 40 ;
  LabeledIntCounter.add query_count ["context","test"; "status","ok"] 2 ;
  LabeledIntCounter.add query_count ["context","test"; "status","nok" ] 7 ;
  let s = BatIO.to_string (LabeledIntCounter.print 1500136019.012674) query_count in
  assert_equal ~printer:BatPervasives.identity
    "# HELP test_counter Demo of a labelled counter\n\
     test_counter{context=\"test\",status=\"ok\"} 42 1500136019012674\n\
     test_counter{context=\"test\",status=\"nok\"} 7 1500136019012674\n\n" s ;

  let response_time =
    LabeledHisto.make "test_histo" "Demo of a labelled histogram" Histogram.(make (linear_buckets 1.)) in
  LabeledHisto.add response_time ["context","test"; "status","ok"] 0.1 ;
  LabeledHisto.add response_time ["context","test"; "status","ok"] 0.12 ;
  LabeledHisto.add response_time ["context","test"; "status","ok"] 1.5 ;
  let s = BatIO.to_string (LabeledHisto.print 1500136019.000001) response_time in
  assert_equal ~printer:BatPervasives.identity
    "# HELP test_histo Demo of a labelled histogram\n\
     test_histo_bucket{le=\"1.\",context=\"test\",status=\"ok\"} 2 1500136019000001\n\
     test_histo_bucket{le=\"2.\",context=\"test\",status=\"ok\"} 3 1500136019000001\n\
     test_histo_sum{context=\"test\",status=\"ok\"} 1.72 1500136019000001\n\
     test_histo_count{context=\"test\",status=\"ok\"} 3 1500136019000001\n\n" s
 *)
