open Batteries
open Assem

module F = Translate.F

let instr_list : instr list ref = ref []

let sprintf = Printf.sprintf

let nop = OP("nop", [], [], None)

let emit instr : unit =
  instr_list := instr :: !instr_list

let assembly_label_string l : string =
  "_" ^ (Temp.label_to_string l)

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
  | LABEL (asm, l) -> (assembly_label_string l) ^ ":"
  | MOVE (asm, dst, src) ->
     begin try asm_str (String.to_list asm) [dst] [src] ""
           with _ ->
             failwith ("error occurs when format asm: " ^
                         asm ^ "\n" ^
                           "dst: " ^ (temp_to_string dst) ^ "\n" ^
                             "src: " ^ (temp_to_string src) ^ "\n")
     end

let binop_to_instr = function
  | Ir.PLUS -> "addl"
  | Ir.MINUS -> "subl"
  | _ -> failwith "NYI"

let relop_to_instr = function
  | Ir.EQ -> "je"
  | Ir.NE -> "jne"
  | Ir.LT -> "jl"
  | Ir.GT -> "jg"
  | Ir.LE -> "jle"
  | Ir.GE -> "jge"
  | _ -> failwith "not applicable in tiger"

(**/ the following describe registers *)

(** registers to which a call replaces its results *)
let call_write_regs = []

let rec munch_exp (exp : Ir.exp) : temp =
  match exp with
  | Ir.CONST(i) ->
    result(fun t ->
        OP("mov " ^ (string_of_int i) ^ ", 'd0", [t], [], None)
        |> emit)
  | Ir.NAME(l) ->
    result(fun t ->
        let l_str = assembly_label_string l in
        emit(OP(sprintf "mov %s, 'd0" l_str, [t], [], None)))
  | Ir.ESEQ (_) -> failwith "ESEQ: This is not canonical IR. Abort"
  | Ir.TEMP(t) -> t
  | Ir.BINOP(op, e0, e1) ->
    let r0 = munch_exp e0 in
    let r1 = munch_exp e1 in
    let instr = binop_to_instr op in
    result(fun t ->
        emit(OP(sprintf "%s 's0, 'd0" instr,
                [t], [r0], None));
        emit(OP(sprintf "%s 's0, 'd0" instr, [t], [r1], None)))
  | Ir.CALL (Ir.NAME(l), args) ->
    let () = munch_args args in
    result(fun t ->
      emit(OP("call " ^ (assembly_label_string l), call_write_regs, [], None)))
  | Ir.MEM (e) ->
    let r0 = munch_exp e in
    result(fun t ->
        emit(OP("mov ('s0), 'd0", [t], [r0], None)))
  | _ -> failwith "NYI"

(* x86-32 passes arguments on stack, so munch_args returns unit *)
and munch_args args : unit =
  List.map munch_exp args
  |> List.iter (fun t ->
      emit(OP("push 's0", [], [t], None)))

and munch_stmt (stmt : Ir.stmt) : unit =
  match stmt with
  | Ir.SEQ (s0, s1) ->
    munch_stmt s0;
    munch_stmt s1
  | Ir.MOVE (Ir.MEM(Ir.BINOP(Ir.PLUS, ir_lhs, Ir.CONST(n))), ir_rhs) ->
    let lhs = munch_exp ir_lhs in
    let v = munch_exp ir_rhs in
    OP(sprintf "mov 's0, %d('s1)" n, [], [v; lhs], None) |> emit
  | Ir.MOVE (Ir.MEM(e), e1) ->
    let src = munch_exp e in
    let moveto = munch_exp e1 in
    (* dst is [], because it is the memory not the reg that holds the value *)
    OP("mov 's0, ('s1)", [], [moveto; src], None)
    |> emit
  | Ir.MOVE (Ir.TEMP(t), e) ->
    let src = munch_exp e in
    MOVE("mov 's0, 'd0", t, src)
    |> emit
  | Ir.EXP(e) ->
    let src = munch_exp e in
    MOVE("xor 's0, 'd0", src, src)
    |> emit
  | Ir.JUMP (Ir.NAME(l), ls) ->
    OP("jmp " ^ (assembly_label_string l), [], [], Some ls)
    |> emit
  | Ir.CJUMP (relop, e0, e1, t, f) -> (* TODO: this is not maximal munch *)
    let t0 = munch_exp e0 in
    let t1 = munch_exp e1 in
    OP("cmp 's0, 's1",
       [], (*NOTE: what is the out register? icc or xcc.
             but we won't use them in register allocation, so ignore it. *)
       [t0; t1], None)
    |> emit;
    OP(sprintf "%s %s"
         (relop_to_instr relop)
         (assembly_label_string t),
       [], [], Some([t; f]))
    |> emit
  | Ir.LABEL(l) ->
    emit(LABEL(assembly_label_string l, l))
  | _ -> failwith ("NYI munch_stmt: " ^ (Ir.stmt_to_string stmt))

and result gen : temp =
  let t = Temp.new_temp () in
  gen t;
  t

let codegen frame ir =
  instr_list := [];
  munch_stmt ir;
  List.rev !instr_list

let codegen_data frags =
  let rec gen_iter frags str_list =
    match frags with
    | [] -> str_list
    | (l, s) :: rest ->
      F.string l s :: gen_iter rest str_list
  in
  (* generate data section content *)
  let data = gen_iter frags [] |> String.concat "\n" in
  (* OK. Now we need section header *)
  let header = [
  ] |> String.concat "\n" in
  header ^ "\n" ^ data
