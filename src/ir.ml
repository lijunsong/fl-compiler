(** This module defines the Intermediate Representation of the tiger
 * compiler. *)

type exp =
  | CONST of int
  | NAME of Temp.label
  | TEMP of Temp.temp
  | BINOP of binop * exp * exp
  | MEM of exp (** memory operation. MEM means store only on the left side of MOVE; it means fetch in other cases *)
  | CALL of exp * exp list (** [function arguments] *)
  | ESEQ of stmt * exp (** evaluate stmt and then return the result of exp *)
 and stmt =
   | MOVE of exp * exp
   | EXP of exp  (** evaluate e and discard the result *)
   | JUMP of exp * Temp.label list (** jump to exp, which has a possible location specified in the list *)
   | CJUMP of relop * exp * exp * Temp.label * Temp.label (** CJUMP(o, e1, e2, t, f), evaluate o(e1, e2), jump to t if true, f if false*)
   | SEQ of stmt * stmt
   | LABEL of Temp.label
 and binop =
   | PLUS | MINUS | MUL | DIV
   | AND | OR | LSHIFT | RSHIFT | ARSHIFT | XOR
 and relop =
   | EQ | NE | LT | GT | LE | GE
   | ULT | ULE | UGT | UGE

(** chain stmts list by SEQ *)
let rec seq stmts : stmt = match stmts with
  | [] -> failwith "seq cannot take empty stmts"
  | hd :: [] -> hd
  | hd :: tl ->
     SEQ(hd, seq tl)
