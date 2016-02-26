open Batteries
open Assem

module F = Translate.F

let instr_list : instr list ref = ref []

let sprintf = Printf.sprintf

let nop = OP("nop", [], [], None)

let emit instr : unit =
  instr_list := instr :: !instr_list

(** TODO *)
let format temp_to_string instr =
  (* todo: asm_str can be made easy if you reverse the template string! *)
  let rec asm_str template dst src str =
    match template with
    | '\'' :: 'd' :: n :: rest ->
       let idx = (int_of_char n) - 48 in
       let temp = List.nth dst idx in
       asm_str rest dst src ((temp_to_string temp |> String.rev) ^ str)
    | '\'' :: 's' :: n :: rest ->
       let idx = (int_of_char n) - 48 in
       let temp = List.nth src idx in
       asm_str rest dst src ((temp_to_string temp |> String.rev) ^ str)
    | hd :: rest ->
       asm_str rest dst src ((String.make 1 hd) ^ str)
    | [] -> String.rev str
  in
  match instr with
  | OP (asm, dst, src, jmp) ->
     begin try asm_str (String.to_list asm) dst src ""
           with _ ->
             failwith ("error occurs when format asm: " ^
                         asm ^ "\n" ^
                           "dst: " ^ (List.map temp_to_string dst
                                      |> String.concat ",") ^ "\n" ^
                             "src: " ^ (List.map temp_to_string src
                                        |> String.concat ",") ^ "\n")
     end
  | LABEL (asm, l) -> asm ^ ":"
  | MOVE (asm, dst, src) ->
     begin try asm_str (String.to_list asm) [dst] [src] ""
           with _ ->
             failwith ("error occurs when format asm: " ^
                         asm ^ "\n" ^
                           "dst: " ^ (temp_to_string dst) ^ "\n" ^
                             "src: " ^ (temp_to_string src) ^ "\n")
     end

let op_to_opcode = function
  | Ir.PLUS -> "add"
  | Ir.MINUS -> "sub"
  | Ir.MUL -> failwith "unavailable on SPARC"
  | Ir.DIV -> failwith "unavailable on SPARC"
  | _ -> failwith "NYI"

(**/ the following describe registers *)

(** registers to which a call replaces its results *)
let call_write_regs =
  List.map F.get_register ["o0"; "o1"; "o2"; "o3"; "o4"; "o5"]
(** zero register on sparc *)
let g0 = F.get_register "g0"
let sp = F.get_register "sp"

(** sparc has 6 input registers, get these registers by their indecies *)
let ireg_of_index i : Temp.temp =
  assert (i >= 0 && i <= 5);
  let reg = sprintf "i%d" i in
  F.get_register reg

let rec munch_exp (exp : Ir.exp) : temp =
  match exp with
  | Ir.CONST(i) ->
     result(fun t ->
         OP("set " ^ (string_of_int i) ^ ", 'd0",
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
     emit(OP("call 's0", call_write_regs, src, None));
     emit(nop);
     result(fun t ->
         emit(MOVE("mov 's0, 'd0", t, F.rv)))
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
         OP ("ld ['s0 + 's1], 'd0", [t], [rand; g0], None)
         |> emit)
  | _ -> failwith ("NYI munch_exp: " ^ (Ir.exp_to_string exp))

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
         emit(OP(sprintf "st 's0, ['s1+%d]" offset,
                 [],
                 [arg_temp; sp],
                 None))
    in
    store_iter args 0
  in
  let rec munch_iter args cur_idx temps =
    if cur_idx > 5 then
      (store_args args; temps)
    else
      match args with
      | [ ] -> temps
      | arg :: rest ->
         let arg_temp = munch_exp arg in
         emit(MOVE("mov 's0, 'd0", ireg_of_index cur_idx, arg_temp));
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
     MOVE("mov 's0, 'd0", g0, src)
     |> emit
  | Ir.JUMP (Ir.NAME(l), ls) ->
     (* Use synthetic*)
     OP("jmp " ^ (Temp.label_to_string l), [], [], Some ls)
     |> emit
  | Ir.CJUMP (relop, e0, e1, t, f) -> (* TODO: this is not maximal munch *)
     let t0 = munch_exp e0 in
     let t1 = munch_exp e1 in
     OP("cmp 's0, 's1",
        [(*TODO: what is the out register?*)],
        [t0; t1], Some([t; f]))
     |> emit
  | Ir.LABEL(l) ->
     emit(LABEL(Temp.label_to_string l, l))
  | _ -> failwith ("NYI munch_stmt: " ^ (Ir.stmt_to_string stmt))

and result gen : temp =
  let t = Temp.new_temp () in
  gen t;
  t

let codegen frame ir =
  munch_stmt ir;
  List.rev !instr_list
