moduel F = Translate.F

type instr

val format: (temp -> string) -> instr -> string

val codegen : F.frame -> Ir.stmt -> instr list
