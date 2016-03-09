type node = {
  def : Temp.temp list;
  use : Temp.temp list;
  ismove : bool;
  mutable succ: node list;
  mutable pred: node list;
  mutable live_out:  Temp.temp list;
} with sexp

type flowgraph = node list with sexp

val instrs2graph : Assem.instr list -> flowgraph

val to_string : flowgraph -> string
