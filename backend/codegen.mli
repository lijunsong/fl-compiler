module F = Translate.F

type instr

type temp = Temp.temp

val format: (temp -> string) -> instr -> string

val codegen : F.frame -> Ir.stmt -> instr list
