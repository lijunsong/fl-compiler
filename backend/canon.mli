(** Description: This is the first phase of normalize IR,
    after linearize.

    - There are no ESEQ and SEQ.
    - Certain subexpressions, like CALL, will be raised to a MOVE or
      EXP statement.
*)

(** Remove ESEQ; move CALL to top level*)
val linearize : Ir.stmt -> Ir.stmt list
