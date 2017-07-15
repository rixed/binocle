type measure = MetricFloat of float
             | MetricInt of int
             | MetricString of string
             | MetricHistogram of (float * float * int) array [@@ppp PPP_OCaml]

(*type metric = {
  name : string ;
  measure : measure ;
} [@@ppp PPP_OCaml]*)

let all_measures : (string, (unit -> measure option)) Hashtbl.t =
  Hashtbl.create 71

(* Add a measure into all_measures and return it. The name given is the name of
 * the family of measures exported *)
let registered name export t =
  assert (not (Hashtbl.mem all_measures name)) ;
  Hashtbl.add all_measures name (fun () -> export t) ;
  t

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
end

module FloatCounter =
struct
  type t = float ref
  type v = float
  let add t v = t := !t +. v
  let set t v = t := v
  let export t = Some (MetricFloat !t)
  let make name = registered name export (ref 0.)
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
end

module StringConst =
struct
  type t = string
  type v = string
  let export t = Some (MetricString t)
  let make name s = registered name export s
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
    counts : (float, float * int) Hashtbl.t }

  (* type of values *)
  type v = float

  let add h v =
    let mi, ma = h.bucket_of_value v in
    match Hashtbl.find h.counts mi with
      | exception Not_found ->
        Hashtbl.add h.counts mi (ma, 1)
      | _, c ->
        Hashtbl.replace h.counts mi (ma, c+1)

  (* Convert the hash into an array of buckets + count *)
  let export h =
    let a = Array.make (Hashtbl.length h.counts) (0., 0., 0) in
    Hashtbl.fold (fun mi (ma, c) i ->
        a.(i) <- (mi, ma, c) ;
        i+1
      ) h.counts 0 |> ignore ;
    Array.sort (fun (mi1, _, _) (mi2, _, _) ->
        compare mi1 mi2
      ) a ;
    Some (MetricHistogram a)

  (* Helpers for bucket_of_value *)
  let linear_buckets ~bucket_size v =
    let mi = floor (v /. bucket_size) *. bucket_size in
    mi, mi +. bucket_size

  let power_of_twos v =
    let ln2 = 0.69314718055994530942 in
    let log2 x = log x /. ln2 in
    let mf = log2 v in
    let flo = floor mf and cei = ceil mf in
    let cei = if cei = flo then cei +. 1. else cei in
    2. ** flo, 2. ** cei

  let make bucket_of_value name =
    registered name export { bucket_of_value ; counts = Hashtbl.create 11 }
end

(* Allows to have arbitrary labels with a metric (by actually maintaining as
 * many metrics as we have encountered label values).  Labels are mere strings,
 * such as "status-ok" or "with-cache-hit". For simplicity these strings
 * encompass both the label name and its value that monitoring system
 * traditionally distinguish, with the only drawback that nothing prevents you
 * from adding a measure with both "status-ok" and "status-error" labels. *)
module Labeled (Measure : sig
    type t
    type v
    val add : t -> v -> unit
    val export : t -> measure option
    val make : string -> t
  end) =
struct
  type t =
    { name : string ;
      per_labels : (string list, Measure.t) Hashtbl.t }

  let tot_name t labels =
    String.concat "-" (t.name :: labels)

  let add t (labels : string list) v =
    let labels = List.fast_sort Pervasives.compare labels in
    match Hashtbl.find t.per_labels labels with
    | exception Not_found ->
      let tot_name = tot_name t labels in
      let m = registered tot_name Measure.export (Measure.make tot_name) in
      Measure.add m v ;
      Hashtbl.add t.per_labels labels m
    | prev ->
      Measure.add prev v

  let make () =
    (* Do not register at once but only when new labels are encountered *)
    Hashtbl.create 17
end
