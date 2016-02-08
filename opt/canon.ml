(** Rearrange IR to Cannoical IR.
    1. Removing ESEQ and SEQ;
    2. Raise CALL expression to a MOVE or EXP statement.
*)

let visit_stmt stmt visitor =
  let children : Ir.exp list = Ir.stmt_children stmt in
  let list : (Ir.exp * Ir.stmt option) = map visitor children in
  let replaced = List.map (fun a,_ -> a) list in
