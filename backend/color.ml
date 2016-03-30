open Liveness
open Debug
open Batteries
module F = Translate.F

type allocation = F.register Temp.TempMap.t

let allocation_to_string a =
  String.concat "\n"
    (Temp.TempMap.enum a
     |> (Enum.map (fun (temp, reg) ->
         Printf.sprintf "%s: %s" (Temp.temp_to_string temp) reg))
     |> List.of_enum

let decrease_degree node : unit =
  match node.status with
  | Ingraph (n) when n > 0 ->
    node.status <- Ingraph (n-1)
  | Ingraph (0) -> failwith (sprintf "node %s has degree 0 already" (Temp.to_string node.temp))
  | _ -> failwith "decrease_degree is applied on a non-ingraph node"


(** Take out all non-precolored node off the graph. *)
let get_color_stack (igraph : igraph) (init : allocation)
    (max_regs : int): Liveness.node list =

  (** check whether all non-precolored nodes are removed *)
  let precolored_left () =
    List.for_all (fun node -> match node.status with
        | Ingraph (_) ->
          Temp.TempMap.mem node.temp init
        | _ -> true) igraph
  in
  let get_worklist () =
    List.filter (fun node -> match node.status with
        | Ingraph (n) -> n < max_regs
        | _ -> false) igraph
  in
  let rec get_iter worklist stack : Temp.temp list =
    match worklist with
    | _ when all_are_precolored () -> stack
    | [] ->
      (* worklist is empty, generate new ones *)
      let new_worklist = get_worklist () in
      if new_worklist = [] then
        failwith "graph can't be colored:\n" (Liveness.to_string igraph)
      else
        get_iter new_worklist stack
    | node :: rest ->
      begin match node.status with
        | Ingraph (degree) ->
          assert (degree < max_regs);
          node.status <- Removed; (* mark node is removed *)
          TempSet.iter decrease_degree node.adj;
          get_iter rest (node.temp :: stack)
        | Removed ->
          get_iter rest stack
  in
  get_iter igraph []

(** Given an igraph, allocated temps, and all usable registers, this
    function colors the igraph with the usable registers *)
let color (igraph : Liveness.igraph)
    (init : allocation)
    (registers : F.register list) : allocation * Temp.temp list =
  let stack = get_color_stack igraph (List.length registers) in
  let get_unused_reg allocated =
    let rec get_iter all_regs =
      match all_regs with
      | [] -> failwith "Can't find unused reg."
      | hd :: rest ->
        if Temp.TempMap.mem hd allocated then
          get_iter rest
        else
          hd
    in
    get_iter registers
  in
  let assign_register stack alloc : allocation =
    match stack with
    | [] -> alloc
    | hd :: rest ->
      let reg = get_unused_reg alloc in
      let alloc' = Temp.TempMap.add hd reg alloc in
      assign_register rest alloc'
  in
  let alloc =   assign_register stack init in
  (if !debug then
     Printf.printf "%s\n" (allocation_to_string alloc));
  alloc, stack
