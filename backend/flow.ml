open Assem
open Batteries
open Util

type node = {
  id : int;  (* identify nodes *)
  def : Temp.TempSet.t;
  use : Temp.TempSet.t;
  ismove : bool;
  mutable succ: node list;
  mutable pred: node list;
  mutable live_in: Temp.TempSet.t;
  (** avoid additional struct to record live_in infor*)
  mutable live_out: Temp.TempSet.t;
}

type flowgraph = node list

module LabelMap = Temp.LabelMap

let id = ref (-1)

let singleton = Temp.TempSet.singleton

let empty = Temp.TempSet.empty

let newid () =
  incr id;
  !id

let node_to_string node =
  let set_to_str set =
    (String.concat "," (List.map Temp.temp_to_string
                          (Temp.TempSet.to_list set)))
  in
  let succid = List.map (fun n->n.id) node.succ in
  let predid = List.map (fun n->n.id) node.pred in
  Printf.sprintf "{id=(%d); pred=(%s); succ=(%s); def=(%s); use=(%s); in=(%s); out=(%s)}"
    node.id
    (String.concat "," (List.map string_of_int predid))
    (String.concat "," (List.map string_of_int succid))
    (set_to_str node.def)
    (set_to_str node.use)
    (set_to_str node.live_in)
    (set_to_str node.live_out)

let to_string graph =
  String.concat "\n" (List.map node_to_string graph)


(** Convert instructions to a directed graph. *)
let instrs2graph instrs : flowgraph =
  (* iterate all instrs and construct a node for each
     instruct. Map label to its position (index) for future
     reference. *)
  let live_in = empty in
  let live_out = empty in
  let rec conv idx instrs nodes map : node list * (int LabelMap.t) =
    match instrs with
    | [] -> List.rev nodes, map
    | OP (ass, def, use, _) :: rest ->
      let nodes' = {id=newid(); def=Temp.TempSet.of_list def;
                    use=Temp.TempSet.of_list use; ismove=false;
                    succ=[]; pred=[]; live_in; live_out} :: nodes in
      conv (idx + 1) rest nodes' map
    | LABEL (ass, l) :: rest ->
      let nodes' = {id=newid(); def=empty;
                    use=empty; ismove=false;
                    succ=[]; pred=[]; live_in; live_out} :: nodes in
      conv (idx + 1) rest nodes' (LabelMap.add l idx map)
    | MOVE (ass, dst, src) :: rest ->
      let nodes' = {id=newid(); def=singleton dst;
                    use=singleton src; ismove=true;
                    succ=[]; pred=[]; live_in; live_out} :: nodes in
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
    (* update succ and update succ's pred.  NOTE: So, we don't need to
     * update node's pred, because we have already updated this node's
       pred during updating succ of previous nodes and succ of other
       jump nodes *)
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
  List.iteri (fun idx (instr, node) ->
      connect idx instr node;
      if !Debug.debug then begin
        let get_register_name t = match Translate.F.get_register_name t with
          | None -> Temp.temp_to_string t
          | Some (r) -> r
        in
        print_string (Codegen_x86.format  get_register_name instr);
        print_string " -> ";
        print_endline (node_to_string node)
      end)
    (List.combine instrs nodes);
  nodes
