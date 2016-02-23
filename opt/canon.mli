(** Remove ESEQ; move CALL to top level*)
val linearize : Ir.stmt -> Ir.stmt list

(** Basic blocks are represented by a list of sequences of stmt.
 * This function also returns an additional label marking the exit
 * of a function *)
val basic_blocks : Ir.stmt list -> Ir.stmt list list * Temp.label

(** Given basic blocks and a exit label, simplify CJUMP so that it is
    followed by its false label. *)
val trace_schedule : Ir.stmt list list * Temp.label -> Ir.stmt list
