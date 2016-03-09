open Assem
open Sexplib.Std
open Sexplib
open Batteries

type node = {
  def : Temp.temp list;
  use : Temp.temp list;
  ismove : bool;
  mutable succ: node list;
  mutable pred: node list;
  mutable live_out:  Temp.temp list;
} with sexp

type flowgraph = node list with sexp

module LabelMap = Map.Make(struct
    type t = Temp.label
    let compare = compare
  end)

(** Convert instructions to a directed graph.
 *
 * Note: theoretically, all instruction except label will always have
 * its previous instruction as pred, but we might sometimes want to
 * feed a non-canonical IR. So I'll always check whether previous IR
 * falls through. *)
let instrs2graph instrs : flowgraph =
  (* iterate all instrs and construct a node for each
     instruct. Map label to its position (index) for future
     reference. *)
  let rec conv idx instrs nodes map : node list * (int LabelMap.t) =
    match instrs with
    | [] -> List.rev nodes, map
    | OP (ass, def, use, _) :: rest ->
      let nodes' = {def; use; ismove=false;
                    succ=[]; pred=[]; live_out=[]} :: nodes in
      conv (idx + 1) rest nodes' map
    | LABEL (ass, l) :: rest ->
      let nodes' = {def=[]; use=[]; ismove=false;
                    succ=[]; pred=[]; live_out=[]} :: nodes in
      conv (idx + 1) rest nodes' (LabelMap.add l idx map)
    | MOVE (ass, dst, src) :: rest ->
      let nodes' = {def=[dst]; use=[src]; ismove=false;
                    succ=[]; pred=[]; live_out=[]} :: nodes in
      conv (idx+1) rest nodes' map
  in
  (* get nodes and labelMap *)
  let nodes, labelMap = conv 0 instrs [] LabelMap.empty in
  let total = List.length nodes in
  (* now, a few helper functions to fetch nodes *)
  let get_node i = List.nth nodes i in
  let add_pred node pred = node.pred <- pred :: node.pred in
  (* OK. start to fill in succ and pred details *)
  let connect idx instr node : unit =
    (* update this instruction's pred if previous instr falls
       through *)
    begin if idx <> 0 then
        match List.nth instrs (idx-1) with
        | OP (_, _, _, Some(_)) -> ()
        | _ -> add_pred node (List.nth nodes (idx-1))
    end;
    (* update succ *)
    let update_succ jmps = match jmps with
      | Some(labs) -> (* get succ from jump *)
        let lab_idx = List.map (fun l -> LabelMap.find l labelMap) labs in
        (* get this instructions succ *)
        let succ = List.map get_node lab_idx in
        (* update succ *)
        node.succ <- succ;
        (* update succ's pred *)
        List.iter (fun n -> add_pred n node) succ
      | None when idx = total-1 -> ()
      | None ->
        let succ = get_node (idx+1) in
        (* succ is its next instruction *)
        node.succ <- [succ];
        (* update succ's pred*)
        add_pred succ node
    in
    match instr with
      | OP (_, _, _, jmps) -> update_succ jmps
      | _ -> update_succ None
  in
  List.iteri (fun idx (instr, node) -> connect idx instr node)
    (List.combine instrs nodes);
  nodes

let node_to_string node =
  Sexp.to_string_hum (sexp_of_node node)

let to_string graph =
  String.concat "\n" (List.map node_to_string graph)
