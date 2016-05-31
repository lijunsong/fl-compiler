val format: (Assem.temp -> string) -> Assem.instr -> string

val select_instr : Arch.frame -> Ir.stmt -> Assem.instr list
