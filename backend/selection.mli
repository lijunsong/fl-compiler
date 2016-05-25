val format: (Assem.temp -> string) -> Assem.instr -> string

val codegen : Arch.frame -> Ir.stmt -> Assem.instr list

val codegen_data : (Temp.label * string) list -> string
(** Given a list string fragments, this function emits backend data section *)
