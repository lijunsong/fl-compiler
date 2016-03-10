type node = {
  id : int;  (* identify nodes *)
  def : Temp.TempSet.t;
  use : Temp.TempSet.t;
  ismove : bool;
  mutable succ: node list;
  mutable pred: node list;
  mutable live_out: Temp.TempSet.t;
}

type flowgraph = node list

val instrs2graph : Assem.instr list -> flowgraph

val to_string : flowgraph -> string
