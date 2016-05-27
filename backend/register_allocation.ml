type allocation = Arch.register Temp.TempMap.t
open Liveness

(** set node's status to Color if node's temp is already mapped to a
    machine register *)
let rec precolor igraph : unit = match igraph with
  | [] -> ()
  | node :: rest ->
    begin match Arch.register_of_temp node.Node.temp with
      | Some (reg) ->
        (* precolored. set the status *)
        node.Node.status := Colored(reg)
      | None -> ()
    end;
    precolor rest


let alloc instr : Assem.instr list * allocation =
  let fgraph = Flow.instrs2graph instr in
  let igraph = flow2igraph fgraph in
  let () = precolor igraph in
  let reg_alloc, temps = Color.color igraph Arch.temp_map Arch.registers in
  instr, reg_alloc
