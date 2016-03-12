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

val instrs2graph : Assem.instr list -> flowgraph

val to_string : flowgraph -> string

val node_to_string : node -> string
