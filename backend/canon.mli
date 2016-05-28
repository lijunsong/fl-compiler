(** Remove ESEQ; move CALL to top level*)
val linearize : Ir.stmt -> Ir.stmt list

(** Given basic blocks and a exit label, simplify CJUMP so that it is
    followed by its false label. *)
val trace_schedule : Basic_block.t list * Temp.label -> Ir.stmt list
