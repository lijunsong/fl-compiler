module F = Translate.F

(** todo: to support multiple backends, this should be a functor. *)

val format: (Assem.temp -> string) -> Assem.instr -> string

val codegen : F.frame -> Ir.stmt -> Assem.instr list

val codegen_data : (Temp.label * string) list -> string
(** Given a list string fragments, this function emits backend data section *)
