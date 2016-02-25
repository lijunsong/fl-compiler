open Batteries

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

let instr_list : instr list ref = ref []

let sprintf = Printf.sprintf

let nop = OP("nop", [], [], None)

let emit instr : unit =
  instr_list := instr :: !instr_list

(** TODO *)
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

(** call instruction will trash some registers, which is defined
here *)
let calldefs = [F.o0; F.o1; F.o2; F.o3; F.o4; F.o5; F.o6]

let rec munch_exp (exp : Ir.exp) : temp =
  match exp with
  | Ir.CONST(i) ->
    result(fun t ->
        OP("set " ^ (string_of_int i) ^ " 'd0",
           [t], [], None)
        |> emit)
  | Ir.NAME(l) -> failwith "unreachable"
  | Ir.ESEQ (_) -> failwith "ESEQ: This is not canonical IR. Abort"
  | Ir.TEMP(t) -> t
  | Ir.BINOP (Ir.MINUS, e, Ir.CONST(n))  (* TODO: redo MINUS.*)
  | Ir.BINOP (Ir.MINUS, Ir.CONST(n), e)  (* SPARC does not have MINUS in addressing mode *)
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
       emit(OP(sprintf "mulx 's0, %d, 'd0" n, [t], [rand], None)))
  | Ir.CALL (f, args) ->
    let src = (munch_exp f) :: (munch_args args) in
    emit(OP("call 's0", calldefs, src, None));
    emit(nop);
    result(fun t ->
        emit(MOVE("mov 's0, 'd0", t, F.O0)))
  | Ir.MEM (Ir.BINOP(Ir.PLUS, e, Ir.CONST(n)))
  | Ir.MEM (Ir.BINOP(Ir.PLUS, Ir.CONST(n), e))
  | Ir.MEM (Ir.BINOP(Ir.MINUS, e, Ir.CONST(n)))
  | Ir.MEM (Ir.BINOP(Ir.MINUS, Ir.CONST(n), e)) ->
    let op = match Ir.get_op exp with
      | Ir.PLUS -> "+"
      | Ir.MINUS -> "-"
      | _ -> failwith "unreachable" in
    let rand = munch_exp e in
    result(fun t ->
        OP(sprintf "ld ['s0 %s %d], 'd0" op n,
           [t], [rand], None)
        |> emit)
  | Ir.MEM (Ir.BINOP(Ir.PLUS, e0, e1))
  | Ir.MEM (Ir.BINOP(Ir.MINUS, e0, e1)) ->
    let rand0 = munch_exp e0 in
    let rand1 = munch_exp e1 in
    let op = match Ir.get_op exp with
      | Ir.PLUS -> "+"
      | Ir.MINUS -> "-"
      | _ -> failwith "unreachable" in
    result(fun t ->
        OP (sprintf "ld ['s0 %s 's1], 'd0" op,
            [t], [rand0; rand1], None)
        |> emit)
  | Ir.MEM (e) ->
    let rand = munch_exp e in
    result(fun t ->
        OP ("ld ['s0 + 's1], 'd0", [t], [rand; F.g0], None)
        |> emit)
  | _ -> failwith ("NYI munch_exp: " ^ (exp_to_string exp))

and munch_args args =
  (** store all arguments on stack (starts on %sp+96) *)
  let store_args args =
    let rec store_iter args count = match args with
      | [] -> ()
      | arg :: rest ->
         let arg_temp = munch_exp arg in
         let offset = 96 + count * F.word_size in
         (** TODO: this is actually not right. The offset should
         stored in a way that callee is aware of these arguments.*)
         emit(OP("st 's0, ['s1+%d]" offset, [], [arg_temp, F.sp]))
    in
    store_iter args 0
  in
  let munch_iter args cur_idx temps =
    if cur_idx > 5 then
      (store_args args; temps)
    else
      match args with
      | [ ] -> temps
      | arg :: rest ->
         let arg_temp = munch_exp arg in
         emit(MOVE("mov 's0, 'd0", F.idx_arg cur_idx, arg_temp));
         munch_iter rest (cur_idx+1) (arg_temp :: temps)
  in
  munch_iter args 0 []

and munch_stmt (stmt : Ir.stmt) : unit =
  match stmt with
  | Ir.SEQ (s0, s1) ->
     munch_stmt s0;
     munch_stmt s1
  | Ir.MOVE (Ir.MEM(Ir.BINOP(Ir.PLUS, Ir.CONST(n), e)), e1)
  | Ir.MOVE (Ir.MEM(Ir.BINOP(Ir.PLUS, e, Ir.CONST(n))), e1) ->
    let moveto = munch_exp e1 in
    let rand = munch_exp e in
    OP(sprintf "st 's0, ['s1 + %d]" n, [], [moveto; rand], None)
    |> emit
  | Ir.MOVE (Ir.MEM(e), e1) ->
    let src = munch_exp e in
    let moveto = munch_exp e1 in
    (* dst is [], because it is the memory not the reg that holds the value *)
    OP("st 's0, ['s1]", [], [moveto; src], None)
    |> emit
  | Ir.MOVE (Ir.TEMP(t), e) ->
     let src = munch_exp e in
     MOVE("mov 's0, 'd0", t, src)
     |> emit
  | Ir.EXP(e) ->
     let src = munch_exp e in
     MOVE("mov 's0, 'd0", F.g0, src)
     |> emit
  | Ir.JUMP (Ir.NAME(l), ls) ->
     (* Use synthetic*)
     OP("jmp " ^ (label_to_string l), [], [], Some ls)
     |> emit
  | Ir.CJUMP (relop, e0, e1, t, f) -> (* TODO: this is not maximal munch *)
     let t0 = munch_exp e0 in
     let t1 = munch_exp e1 in
     OP("cmp 's0, 's1",
        [(*TODO: what is the out register?*)],
        [t0, t1], Some([t; f]))
     |> emit
  | Ir.LABEL(l) ->
     LABEL("label " ^ (label_to_string l), l)
  | _ -> failwith ("NYI munch_stmt: " ^ (Ir.exp_to_string exp))

and result gen : temp =
  let t = Temp.new_temp () in
  gen t;
  t

let codegen frame ir =
  munch_stmt ir;
  List.rev !instr_list
