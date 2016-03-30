module F = Translate.F
type allocation = F.register Temp.TempMap.t

let alloc instr : Assem.instr list * allocation =
  let fgraph = Flow.instr2graph instr in
  let igraph = Liveness.flow2igraph fgraph in
  let reg_alloc, temps = Color.color igraph F.reg_allocation F.registers in
  instr, reg_alloc
