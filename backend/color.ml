open Liveness
open Debug
open Batteries
open Printf


type allocation = Arch.register Temp.TempMap.t

let allocation_to_string a =
  String.concat "\n"
    (Temp.TempMap.enum a
     |> (Enum.map (fun (temp, reg) ->
         Printf.sprintf "%s: %s" (Temp.temp_to_string temp) reg))
     |> List.of_enum)

(** Take out all non-precolored node off the graph. This function has
    side effect that modifies status of igraph nodes to Removed. *)
let get_color_stack (igraph : igraph) (init : allocation)
    (max_regs : int): Node.t list =

  (** check whether all non-precolored nodes are removed *)
  let all_removed () =
    List.for_all (fun node -> match !(node.Node.status) with
        | Ingraph (_) ->
          Temp.TempMap.mem node.Node.temp init
        | _ -> true) igraph
  in
  let get_worklist () =
    List.filter (fun node -> match !(node.Node.status) with
        | Ingraph (n) -> n < max_regs
        | _ -> false) igraph
  in
  let rec get_iter worklist stack : Node.t list =
    match worklist with
    | _ when all_removed () -> stack
    | [] ->
      (* worklist is empty, generate new ones *)
      let new_worklist = get_worklist () in
      if new_worklist = [] then
        failwith ("graph can't be colored:\n" ^ (Liveness.to_string igraph))
      else
        get_iter new_worklist stack
    | node :: rest ->
      begin match !(node.Node.status) with
        | Ingraph (degree) ->
          assert (degree < max_regs);
          (* get adj that is still in graph *)
          let adj = NodeSet.filter (fun n -> match !(n.Node.status) with
              | Ingraph (_) -> true
              | _ -> false) node.Node.adj in
          let () = NodeSet.iter decrease_degree adj in
          node.Node.status := Removed; (* mark node is removed *)
          get_iter rest (node :: stack)
        | Removed ->
          get_iter rest stack
        | _ -> failwith "get_color_stack: unreachable"
      end
  in
  let init_worklist = get_worklist () in
  get_iter init_worklist []

(** Given an igraph, allocated temps, and all usable registers, this
    function colors the igraph with the usable registers *)
let color (igraph : Liveness.igraph)
    (init : allocation)
    (registers : Arch.register list) : allocation * Temp.temp list =
  let stack = get_color_stack igraph init (List.length registers) in
  let get_unused_reg cur_node (allocated : Arch.register Temp.TempMap.t) : Arch.register =
    (* use adj list to filter all nodes whose status is Colored(reg) *)
    let used_regs = (let nodes = NodeSet.enum cur_node.Node.adj
                                 |> List.of_enum in
                     List.fold_right (fun n res -> match !(n.Node.status) with
                         | Colored(reg) -> reg :: res
                         | _ -> res) nodes []) in
    let rec get_iter (all_regs : Arch.register list) =
      match all_regs with
      | [] -> failwith "Can't find unused reg."
      | hd :: rest ->
        if List.mem hd used_regs then
          get_iter rest
        else
          hd
    in
    get_iter registers
  in
  (** Given a stack, assign elements in the stack registers. This
      function will modify nodes' status to Colored(reg) *)
  let rec assign_register (stack : Node.t list) alloc : allocation =
    match stack with
    | [] -> alloc
    | hd :: rest ->
      let reg = get_unused_reg hd alloc in
      let alloc' = Temp.TempMap.add hd.Node.temp reg alloc in
      hd.Node.status := Colored(reg);
      assign_register rest alloc'
  in
  let alloc =   assign_register stack init in
  (if !debug then
     Printf.printf "%s\n" (allocation_to_string alloc));
  alloc, List.map (fun n -> n.Node.temp) stack
