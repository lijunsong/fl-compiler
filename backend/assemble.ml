type temp = Temp.temp

type label = Temp.label

type instr =
  | OP of string * temp list * temp list * label list option
  (** assembly, dst, src, jump *)

  | LABEL of string * label
  (** assembly, label *)

  | MOVE of string * temp * temp
  (** assembly, dst, src *)

module F = Translate.F

let instr_list : instr list = ref []

let sprintf = Printf.sprintf

let nop = OP("nop", [], [], None)

let emit instr : unit =
  instr_list := instr :: !instr_list

let to_string = function
  | OP (_) -> "op"
  | LABEL (_) -> "label"
  | MOVE (_) -> "move"

let format tmp_to_string instr =
  match instr with
  | OP (asm, dst, srcs, jmp) -> asm
  | LABEL (asm, l) -> asm
  | MOVE (asm, dst, src) -> asm

let op_to_opcode = function
  | Ir.PLUS -> "add"
  | Ir.MINUS -> "sub"
  | Ir.MUL -> failwith "unavailable on SPARC"
  | Ir.DIV -> failwith "unavailable on SPARC"
  | _ -> failwith "NYI"

let rec munch_exp (exp : Ir.exp) : temp =
  match exp with
  | Ir.CONST(i) ->
    result(fun t ->
        OP("set " ^ (string_of_int i) ^ " 'd0",
           [t], [], None)
        |> emit)

    |> emit
  | Ir.NAME(l) -> failwith "unreachable"
  | Ir.ESEQ -> failwith "ESEQ: This is not canonical IR. Abort"
  | Ir.TEMP(t) -> t
  | Ir.BINOP (Ir.MINUS, e, Ir.CONST(n))
  | Ir.BINOP (Ir.MINUS, Ir.CONST(n), e)
  | Ir.BINOP (Ir.PLUS, e, Ir.CONST(n))
  | Ir.BINOP (Ir.PLUS, Ir.CONST(n), e) ->
    let opcode = Ir.get_op exp |> op_to_opcode in
    let rand = munch_exp e in
    if (-4096) <= n && n >= 4095 then
      result(fun t ->
          OP(sprintf "%s 's0, %s, 'd0" opcode (string_of_int n),
             [t], [rand], None)
          |> emit)
    else
      let rand1 = munch_exp (Ir.CONST(n)) in
      result(fun t ->
          OP(sprintf "%s 's0, 's1, 'd0" opcode,
             [t], [rand; rand1], None)
          |> emit)

  | Ir.BINOP (Ir.MUL, Ir.CONST(n), e)
  | Ir.BINOP (Ir.MUL, e, Ir.CONST(n)) ->
    let rand = munch_exp e in (* TODO: desugar this to a CALL? *)
    result(fun t ->
        (* mov _ %O0 *)
        emit(OP(sprintf "mov %s, 'd0" (string_of_int n), [F.O0], [], None));
        (* mov _ %O1 *)
        emit(OP("mov 's0, 'd0", [F.O1], [e], None));
        (* call .mult *)
        emit(OP("call .mult", [], [], None));
        emit(nop);
        emit(OP("mov 's0, 'd0", [F.I0], [t], None)))
  | Ir.CALL (f, args) ->
    let src = (much_exp f) :: (munch_args args) in
    emit(OP("call 's0", calldefs, src, None));
    emit(nop);
    result(fun t ->
        emit(OP("mov 's0, 'd0", [t], [F.O0], None)))
  | Ir.MEM ()
  | _ -> failwith ("NYI: " ^ (exp_to_string exp))

and rec munch_stmt (stmt : Ir.stmt) : unit =
  match stmt with
  | SEQ (_) -> failwith "found SEQ. This is Canonical IR. Abort"
  | MOVE ()



and result gen : temp =
  let t = Temp.new_temp () in
  gen t;
  t
