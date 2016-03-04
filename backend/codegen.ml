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

let op_to_instr = function
  | Ir.PLUS -> "add"
  | Ir.MINUS -> "sub"
  | Ir.MUL -> "smul"
  | Ir.DIV -> "sdiv"
  | _ -> failwith "not applicable in tiger"

(**/ the following describe registers *)

(** registers to which a call replaces its results *)
let call_write_regs = []
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
         OP("mov " ^ (string_of_int i) ^ ", 'd0", [t], [], None)
         |> emit)
  | Ir.NAME(l) -> failwith "unreachable"
  | Ir.ESEQ (_) -> failwith "ESEQ: This is not canonical IR. Abort"
  | Ir.TEMP(t) -> t
  | Ir.BINOP(op, e0, e1) ->
    let r0 = munch_exp e0 in
    let r1 = munch_exp e1 in
    let instr = op_to_instr op in
    result(fun t ->
        emit(OP(sprintf "%s 's0, 's1, 'd0" instr,
               [t], [r0; r1], None)))
  | Ir.CALL (Ir.NAME(l), args) ->
     let src = munch_args args in
     emit(OP("call " ^ (Temp.label_to_string l), call_write_regs, src, None));
     emit(nop);
     result(fun t ->
         emit(MOVE("mov 's0, 'd0", t, F.rv)))
  | Ir.CALL (f, args) -> (* This one seems not right. *)
     let src = munch_exp f :: munch_args args in
     emit(OP("call 's0" , call_write_regs, src, None));
     emit(nop);
     result(fun t ->
         emit(MOVE("mov 's0, 'd0", t, F.rv)))

  | Ir.MEM (e) ->
     let rand = munch_exp e in
     result(fun t ->
         OP ("ld ['s0], 'd0", [t], [rand], None)
         |> emit)

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
