(** Remove ESEQ; move CALL to top level*)
val linearize : Ir.stmt -> Ir.stmt list

(** Basic blocks are represented by a list of sequences of stmt.
 * TODO: why additional label? *)
val basic_blocks : Ir.stmt list -> Ir.stmt list list * Temp.label

(** Given basic blocks, simplify CJUMP so that it is followed by its
    false label. *)
val trace_schedule : Ir.stmt list list * Temp.label -> Ir.stmt list
