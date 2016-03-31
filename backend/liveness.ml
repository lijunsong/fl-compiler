(** Variable convention: nodes from flow graph is called fnode; nodes
    from interference graph is called inode. *)

open Temp
open Batteries
open Printf

type status =
  | Ingraph of int
  | Removed
  | Colored of string

module rec Node : sig
  type t = {
    temp: temp;
    mutable adj: NodeSet.t;
    mutable status : status ref;
  }
  val compare : t -> t -> int
end = struct
  type t = {
    temp: temp;
    mutable adj: NodeSet.t;
    mutable status : status ref;
  }
  let compare a b = compare a.temp b.temp
end
and NodeSet : (Set.S with type elt = Node.t) = Set.Make(Node)

type igraph = Node.t list

(* a set for cache fnode *)
module IdSet = Set.Make (struct
    type t = int
    let compare = compare
  end)

let status_to_string = function
  | Ingraph (n) -> sprintf "Ingraph(%d)" n
  | Removed -> "Removed"
  | Colored (reg) -> sprintf "Colored(%s)" reg

let node_to_string node =
  Printf.sprintf "%s (%s): %s" (Temp.temp_to_string node.Node.temp)
    (status_to_string !(node.Node.status))
    (String.concat ", " (List.map Temp.temp_to_string
                           (List.map (fun n -> n.Node.temp)
                              (NodeSet.to_list node.Node.adj))))

let to_string igraph =
  String.concat "\n" (List.map node_to_string igraph)

let decrease_degree (node : Node.t) : unit =
  match ! (node.Node.status) with
  | Ingraph (n) when n > 0 ->
    node.Node.status := Ingraph (n-1)
  | Ingraph (0) -> failwith (sprintf "node %s has degree 0 already" (Temp.temp_to_string node.Node.temp))
  | _ -> failwith ("decrease_degree is applied on a non-ingraph node: " ^ (node_to_string node))

let increase_degree (node : Node.t) : unit =
  match ! (node.Node.status) with
  | Ingraph(n) ->
    node.Node.status := Ingraph(n+1)
  | _ -> failwith ("increase_degree is applied on a non-ingraph node: " ^ (node_to_string node))

(* union across a list of set *)
let union_list (list : TempSet.t list) =
  List.fold_right (fun elt init -> TempSet.union elt init) list TempSet.empty

let set_to_str set =
  (String.concat "," (List.map temp_to_string
                        (TempSet.to_list set)))

(** Helper function: Compute IN and OUT set:
 *
 * OUT = Sum succ's IN
 * IN  = use + (OUT - def)
 *
 * To make it converge fast, compute from the last node backwords
 * following predecessors and compute OUT before IN.
 *
 * TODO: Is it possible that this trace process would miss out any node?
 *)

let rec trace_liveout fnodes : unit =
  (* compute OUT and IN. return true if any info changed, false otherwise. *)
  let fnode_liveness (fnode : Flow.node) : bool =
    let succ_livein = List.map (fun n->n.Flow.live_in) fnode.Flow.succ in
    (* compute OUT first and then IN *)
    let new_out = union_list succ_livein in
    let new_in = TempSet.union fnode.Flow.use
        (TempSet.diff new_out fnode.Flow.def) in
    if !Debug.debug then
      Printf.printf "for node %s: \nnew_in: %s\nnew_out: %s\n"
        (Flow.node_to_string fnode) (set_to_str new_in) (set_to_str new_out);
    if TempSet.equal new_in fnode.Flow.live_in &&
       TempSet.equal new_out fnode.Flow.live_out then
      false
    else begin
      fnode.Flow.live_out <- new_out;
      fnode.Flow.live_in <- new_in;
      true
    end
  in
  let rec trace_iter stack visited changed : bool =
    match stack with
    | [] -> changed
    | fnode :: rest when IdSet.mem fnode.Flow.id visited ->
      trace_iter rest visited changed
    | fnode :: rest ->
      if !Debug.debug then
        print_endline ("visit: " ^ Flow.node_to_string fnode);
      (* NOTE: be aware of the short circuit. We need fnode_liveness's
         side effect. *)
      let changed' = fnode_liveness fnode || changed in
      let stack' = fnode.Flow.pred @ rest in
      let visited' = IdSet.add fnode.Flow.id visited in
      trace_iter stack' visited' changed'
  in
  (* start from the pred of the last flowgraph node *)
  let last = List.last fnodes in
  (* OK, now compute until we reach a fix point. *)
  if trace_iter last.Flow.pred IdSet.empty false then
    begin
      (if !Debug.debug then
         print_endline ("--GRAPH---\n" ^ (Flow.to_string fnodes) ^ "\n---DONE---"));
      trace_liveout fnodes
    end
  else ()


(**/ interference graph functions *)

(** Helper function: given a set of def temps, live_out temps and
    pool, return inodes for def temps, live_out, and a new pool *)
let new_inode (defs : TempSet.t) (live_out : TempSet.t) (pool : Node.t TempMap.t) =
  (* iteration to get inodes and pool. *)
  let rec temps_to_inodes (temps : temp list) inodes pool : Node.t list * Node.t TempMap.t =
    match temps with
    | [] -> inodes, pool
    | temp :: rest ->
      if TempMap.mem temp pool then
        let node = TempMap.find temp pool in
        temps_to_inodes rest (node :: inodes) pool
      else
        (* if this node is precolored (the color was decided in
           munch_expr), we still use Ingraph as the status to avoid
           dependencies on Frame. We can always set the correct
           status later.*)
        let node = { Node.temp = temp;
                     Node.adj = NodeSet.empty;
                     Node.status = ref (Ingraph 0) } in
        let pool' = TempMap.add temp node pool in
        temps_to_inodes rest (node :: inodes) pool'
  in
  let def, pool' = temps_to_inodes (TempSet.to_list defs) [] pool in
  let live_out, pool'' = temps_to_inodes (TempSet.to_list live_out) [] pool' in
  def, live_out, pool''

(* add edges between two sets, given by nodes0 and nodes1 *)
let add_edge (nodes0 : Node.t list) (nodes1 : Node.t list) : unit =
  if !Debug.debug then
    printf "add_edge: [%s] <-> [%s]\n"
      (String.concat ","
         (List.map (fun n -> "{" ^ (node_to_string n) ^ "}") nodes0))
      (String.concat ","
         (List.map (fun n -> "{" ^ (node_to_string n) ^ "}") nodes1));
  let rec add nodes =
    match nodes with
    | [] -> ()
    | src :: rest ->
      List.iter (fun dst ->
          if not (src.Node.temp = dst.Node.temp) then begin
            src.Node.adj <- NodeSet.add dst src.Node.adj;
            dst.Node.adj <- NodeSet.add src dst.Node.adj;
            increase_degree src;
            increase_degree dst
          end) nodes1;
      add rest
  in
  if nodes0 = [] || nodes1 = [] then
    ()
  else
    add nodes0


(** Helper function: compute interference graph from given flow graph
    where live_out info has been filled. *)
let get_igraph fnodes : igraph =
  let rec make_graph fnodes temp_pool : Node.t TempMap.t =
    match fnodes with
    | [] -> temp_pool
    | fnode :: rest ->
      (* get the corresponding inode for def and live_out *)
      let def_inodes, live_out_inodes, temp_pool' =
        new_inode fnode.Flow.def fnode.Flow.live_out temp_pool
      in
      (* filter out the use of a move from the live_out_inodes: a <- c
         does not interfere.  TODO: when coalscing is implemented, we
         can claim that a<-c does not interfere*)
      (*let live_out_inodes' = if not fnode.Flow.ismove then
          live_out_inodes
        else*)
      let live_out_temps = TempSet.diff fnode.Flow.live_out fnode.Flow.use in
      let live_out_inodes' = List.map (fun temp ->
          TempMap.find temp temp_pool') (TempSet.to_list live_out_temps)
      in
      (* add an edge between def and live_out *)
      add_edge def_inodes live_out_inodes';
      make_graph rest temp_pool'
  in
  let pool = make_graph fnodes TempMap.empty in
  TempMap.values pool
  |> List.of_enum

(** Compute interference graph (igraph): compute live out information
    before computing interference graph. *)
let flow2igraph (flowg : Flow.flowgraph) : igraph =
  trace_liveout flowg;
  get_igraph flowg
