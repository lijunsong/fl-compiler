(** This module defines the Intermediate Representation (IR) of the
    tiger compiler. *)

type exp =
  | CONST of int
  | NAME of Temp.label
  | TEMP of Temp.temp
  | BINOP of binop * exp * exp
  | MEM of exp
  (** memory operation. MEM means store only on the left side of MOVE;
      it means fetch in other cases *)
  | CALL of exp * exp list
  (** [function arguments] *)
  | ESEQ of stmt * exp
  (** evaluate stmt and then return the result of exp *)
and stmt =
  | MOVE of exp * exp (** dst, src *)
  | EXP of exp  (** evaluate e and discard the result *)
  | JUMP of exp * Temp.label list
  (** jump to exp, which has a possible location specified in the
      list *)
  | CJUMP of relop * exp * exp * Temp.label * Temp.label
  (** CJUMP(o, e1, e2, t, f), evaluate o(e1, e2), jump to t if true, f
      if false*)
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
  | [] -> EXP(CONST(0))
  | hd :: [] -> hd
  | hd :: tl ->
    SEQ(hd, seq tl)

let rec exp_to_string e = match e with
  | CONST n -> string_of_int n
  | NAME l -> Temp.label_to_string l
  | TEMP t -> Temp.temp_to_string t
  | BINOP (op, e0, e1) ->
    Printf.sprintf "(%s %s %s)"
      (exp_to_string e0) (binop_to_string op) (exp_to_string e1)
  | MEM m -> Printf.sprintf "[%s]" (exp_to_string m)
  | CALL (f, args) ->
    Printf.sprintf "%s(%s)" (exp_to_string f)
      (String.concat ", " (List.map exp_to_string args))
  | ESEQ (s, e) ->
    (stmt_to_string s) ^ (exp_to_string e)

and stmt_to_string stmt =
  let endl = ";\n" in
  match stmt with
  | MOVE (dst, src) ->
    (exp_to_string dst) ^ " := " ^ (exp_to_string src) ^ ";\n"
  | EXP e -> (exp_to_string e) ^ endl;
  | JUMP (e, ls) ->
    Printf.sprintf "goto %s; # %s"
      (exp_to_string e)
      (String.concat ", " (List.map (fun l -> Temp.label_to_string l) ls)) ^
    endl
  | CJUMP (op, e0, e1, t, f) ->
    Printf.sprintf "if %s %s %s then %s else %s;" (exp_to_string e1)
      (relop_to_string op) (exp_to_string e1) (Temp.label_to_string t)
      (Temp.label_to_string f)
  | SEQ (s1, s2) ->
    (stmt_to_string s1) ^ endl ^ (stmt_to_string s2) ^ endl
  | LABEL (l) ->
    (Temp.label_to_string l) ^ ":\n"

and binop_to_string = function
  | PLUS -> "+"
  | MINUS -> "-"
  | MUL -> "x"
  | DIV -> "/"
  | _ -> failwith "NYI"
and relop_to_string = function
  | EQ -> "="
  | NE -> "<>"
  | LT -> "<"
  | GT -> ">"
  | LE -> "<="
  | GE -> ">="
  | _ -> failwith "NYI"

let get_op = function
  | BINOP (op, _, _) -> op
  | _ -> failwith "unreachable"
