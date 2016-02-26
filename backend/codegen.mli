module F = Translate.F

val format: (Assem.temp -> string) -> Assem.instr -> string

val codegen : F.frame -> Ir.stmt -> Assem.instr list
