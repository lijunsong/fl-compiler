(** Variable convention: nodes from flow graph is called fnode; nodes
    from interference graph is called inode. *)

open Temp
open Batteries

type node = {
  temp: temp;
  mutable adj: TempSet.t;
}

type igraph = node list

type fnode = {
  flownode: Flow.node;
  mutable live_in: TempSet.t
}
(** fnode is for computing live in and live out information. *)

(* a set for cache fnode *)
module IdSet = Set.Make (struct
    type t = int
    let compare = comapre
  end)

(* union across a list of set *)
let union_list (list : IdSet.t list) =
  IdSet.fold (fun elt init -> IdSet.union elt init) list

(** Helper function: Compute IN and OUT set:
 *
 * OUT = Sum pred's IN
 * IN  = use + (OUT - def)
 *
 * To make it converge fast, compute from the last node backwords
 * following predecessors and compute OUT before IN.
 *
 * TODO: Is it possible that this trace process would miss out any node?
 *)

let rec trace_liveout fnodes : unit =
  (* compute OUT and IN. return true if any info changed, false otherwise. *)
  let fnode_liveness fnode : bool =
    let pred_livein = IdSet.map (fun fnode->fnode.live_in) fnode.flownode.pred in
    (* compute OUT first and then IN *)
    let new_out = union_list pred_livein in
    let new_in = IdSet.union fnode.flownode.use
        (IdSet.diff new_out fnode.flownode.def) in
    if IdSet.equal new_in fnode.live_in &&
       IdSet.equal new_out fnode.flownode.live_out then
      false
    else begin
      fnode.flownode.live_out <- new_out;
      fnode.live_in <- new_in;
      true
    end
  in
  let trace_iter stack visited changed : bool =
    match stack with
    | [] -> changed
    | fnode :: rest when IdSet.mem fnode.flownode.id visited ->
      trace_iter rest visited changed
    | fnode :: rest ->
      let changed' = changed || fnode_liveness fnode in
      let stack' = fnode.flownode.pred @ rest in
      let visited' = IdSet.add fnode.flownode.id visited in
      trace_iter stack' visited' changed'
  in
  (* start from the pred of the last flowgraph node *)
  let last = List.last fnodes in
  (* OK, now compute until we reach a fix point. *)
  if trace_iter last.flownode.pred IdSet.empty false then
    trace_liveout fnodes
  else ()


(**/ interference graph functions *)

(** Helper function: given a set of def temps, live_out temps and
    pool, return inodes for def temps, live_out, and a new pool *)
let new_inode (defs : TempSet.t) (live_out : TempSet.t) (pool : TempMap.t) =
  (* iteration to get inodes and pool. *)
  let rec temps_to_inodes (temps : temp list) inodes pool : node list * pool =
    match temps with
    | [] -> inodes, pool
    | temp :: rest ->
      if TempMap.mem temp pool then
        let node = TempMap.find temp pool in
        temps_to_inodes rest (node :: inodes) pool
      else
        let node = { temp = temp; adj = TempSet.empty } in
        let pool' = TempMap.add temp node pool in
        temps_to_inodes rest (node :: inodes) pool'
  in
  let def, pool' = temps_to_inodes (TempSet.to_list defs) [] pool in
  let live_out, pool'' = temps_to_inodes (TempSet.to_list live_out) [] pool' in
  def, live_out, pool''

(* add edges between two sets, given by nodes0 and nodes1 *)
let add_edge nodes0 nodes1 =
  let rec add nodes =
    match nodes with
    | [] -> ()
    | src :: rest ->
      List.iter (fun dst ->
          src.adj <- TempSet.add dst src.adj;
          dst.adj <- TempSet.add src dst.adj;) nodes1;
      add rest nodes1
  in
  if nodes0 = [] || nodes1 = [] then
    ()
  else
    add nodes0


(** Helper function: compute interference graph from given flow graph
    where live_out info has been filled. *)
let get_igraph fnodes : igraph =
  let make_graph fnodes temp_pool : temp_pool =
    match fnodes with
    | [] -> temp_pool
    | fnode :: rest ->
      (* get the corresponding inode for def and live_out *)
      let def_inodes, live_out_inodes, temp_pool' =
        new_inode fnode.flownode.def fnode.flownode.live_out temp_pool
      in
      (* add an edge between def and live_out *)
      add_edges def_inodes live_out_inodes;
      make_graph rest temp_pool'
  in
  let pool = make_graph fnodes TempMap.empty in
  TempMap.values map
  |> List.of_enum

(** Compute interference graph (igraph): compute live out information
    before computing interference graph. *)
let flow2igraph (flowg : Flow.node) : igraph =
  (* add live_in in flow graph *)
  let fnodes = List.map (fun flownode->
    {flownode; live_in=TempSet.empty}) flowg in
  trace_liveout fnodes;
  get_igraph fnodes
