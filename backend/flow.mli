type node = {
  def : Temp.temp list;
  use : Temp.temp list;
  ismove : bool;
  mutable succ: node list;
  mutable pred: node list;
  mutable liveOut:  Temp.temp list;
}

type flowgraph = node list

val instrs2graph : Assem.instr list -> flowgraph
