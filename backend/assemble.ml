type temp = Temp.temp

type label = Temp.label

type instr =
  | OP of string * temp list * temp list * label list option
  (** assembly, dst, src, jump *)

  | LABEL of string * label
  (** assembly, label *)

  | MOVE of string * temp * temp
  (** assembly, dst, src *)

let format tmp_to_string instr =
  match instr with
  | OP (asm, dst, srcs, jmp) -> asm
  | LABEL (asm, l) -> asm
  | MOVE (asm, dst, src) -> asm

let rec munch_exp (exp : Ir.exp) : temp =
  match exp with
  | Ir.CONST(i) ->
    result(fun t ->
        OP("set " ^ (string_of_int i) ^ " 'd0",
           [t], [], None))
  | Ir.NAME(l) -> LABEL(label_to_string l, l)
  | Ir.TEMP(t) -> t
  | Ir.BINOP (op, l, r) ->


and result (gen : temp -> unit) : temp =
  let t = Temp.new_temp () in
  gen t;
  t
