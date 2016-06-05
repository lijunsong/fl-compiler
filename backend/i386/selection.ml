open Batteries
open Assem

let instr_list : instr list ref = ref []

let sprintf = Printf.sprintf

let nop = OP("nop", [], [], None)

let emit instr : unit =
  instr_list := instr :: !instr_list

let emit_comment_exp exp : unit =
  if !Debug.debug = false then
    ()
  else
    let s = Ir.exp_to_string exp
            |> String.replace_chars (fun c ->
                if c = '\n' then " "
                else String.of_char c ) in
    instr_list := OP("; " ^ s, [], [], None) :: !instr_list

let emit_comment_stmt stmt : unit =
  if !Debug.debug = false then
    ()
  else
    let s = Ir.stmt_to_string stmt
            |> String.replace_chars (fun c ->
                if c = '\n' then " "
                else String.of_char c ) in
    instr_list := OP("; " ^ s, [], [], None) :: !instr_list

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
  | Ir.MUL -> "imull"
  | _ -> failwith "NYI binop_to_instr"

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
let eax = Arch.temp_of_register "%eax"
let call_write_regs = List.map Arch.temp_of_register Arch.caller_save
let sp = Arch.temp_of_register "%esp"

let rec munch_exp (exp : Ir.exp) : temp =
  match exp with
  | Ir.CONST(i) ->
    result(fun t ->
        OP("mov $" ^ (string_of_int i) ^ ", 'd0", [t], [], None)
        |> emit)
  | Ir.NAME(l) ->
    result(fun t ->
        let l_str = assembly_label_string l in
        emit(OP(sprintf "mov $%s, 'd0" l_str, [t], [], None)))
  | Ir.ESEQ (_) -> failwith "ESEQ: This is not canonical IR. Abort"
  | Ir.TEMP(t) -> t
  | Ir.BINOP(op, e0, e1) ->
    let r0 = munch_exp e0 in
    let r1 = munch_exp e1 in
    result(fun t ->
        emit(MOVE("mov 's0, 'd0", t, r0));
        emit(OP(sprintf "%s 's0, 'd0" (binop_to_instr op), [t], [r1; t], None)))
  | Ir.CALL (Ir.NAME(l), args) ->
    (* Caveat: CALL could be external calls (like calls in runtime),
       or user defined tiger function calls. Keep name as it is
       here. Any mangled name should be done before the NAME is
       generated. *)
    (* Caveat 2: after call, we need to manually unwind the stack. *)
    let () = munch_args args in
    result(fun t ->
        (* Caveat 3: it is appealing to put eax as a dest reg
           here. But this t represents the result of the call, and
           will be used by others. So generate an extra call move
           from eax to t.
        *)
        emit(OP("call " ^ (assembly_label_string l), [t], [], None));
        emit(MOVE("movl 's0, 'd0", Arch.rv, t)))
  | Ir.MEM (Ir.BINOP(Ir.PLUS, Ir.TEMP(r), Ir.CONST(n)))
  | Ir.MEM (Ir.BINOP(Ir.PLUS, Ir.CONST(n), Ir.TEMP(r))) ->
    result (fun t -> emit(OP(sprintf "movl %d('s0), 'd0" n, [t], [r], None)))

  | Ir.MEM (Ir.BINOP(Ir.PLUS, Ir.CONST(n), e))
  | Ir.MEM (Ir.BINOP(Ir.PLUS, e, Ir.CONST(n))) ->
    let r = munch_exp e in
    result (fun t -> emit(OP(sprintf "movl %d('s0), 'd0" n, [t], [r], None)))

  | Ir.MEM (e) ->
    let r0 = munch_exp e in
    result(fun t -> emit(OP("movl ('s0), 'd0", [t], [r0], None)))
  | _ -> failwith "NYI"

(* x86-32 passes arguments on stack, so munch_args returns unit.
   The right most argument is pushed first.
 *)
and munch_args args : unit =
  List.iteri (fun i arg -> match arg with
      | Ir.CONST(n) ->
        emit(OP(sprintf "movl $%d, %d(%%esp)" n (i*4), [], [], None))
      | e ->
        let t = munch_exp e in
        emit(OP(sprintf "movl 's0, %d(%%esp)" (i*4), [], [t], None))
    ) args


and munch_stmt (stmt : Ir.stmt) : unit =
  match stmt with
  | Ir.SEQ (s0, s1) ->
    munch_stmt s0;
    munch_stmt s1
  | Ir.MOVE (Ir.MEM(Ir.BINOP(Ir.PLUS, ir_rand, Ir.CONST(n))), Ir.CONST(c))
  | Ir.MOVE (Ir.MEM(Ir.BINOP(Ir.PLUS, Ir.CONST(n), ir_rand)), Ir.CONST(c)) ->
    (* move const to memory *)
    let rand = munch_exp ir_rand in
    OP(sprintf "movl $%d, %d('s0)" c n, [], [rand], None) |> emit

  | Ir.MOVE (Ir.MEM(Ir.BINOP(Ir.PLUS, ir_lhs, Ir.CONST(n))), ir_rhs)
  | Ir.MOVE (Ir.MEM(Ir.BINOP(Ir.PLUS, Ir.CONST(n), ir_lhs)), ir_rhs) ->
    (* move others to memory *)
    let lhs = munch_exp ir_lhs in
    let v = munch_exp ir_rhs in
    OP(sprintf "movl 's0, %d('s1)" n, [], [v; lhs], None) |> emit

  | Ir.MOVE (Ir.TEMP(r), Ir.MEM(Ir.BINOP(Ir.PLUS, Ir.TEMP(lhs), Ir.CONST(n))))
  | Ir.MOVE (Ir.TEMP(r), Ir.MEM(Ir.BINOP(Ir.PLUS, Ir.CONST(n), Ir.TEMP(lhs)))) ->
    (* move from memory (reg calculation) to register *)
    OP(sprintf "movl %d('s0), 'd0" n, [r], [lhs], None) |> emit

  | Ir.MOVE (Ir.TEMP(r), Ir.MEM(Ir.BINOP(Ir.PLUS, ir_lhs, Ir.CONST(n))))
  | Ir.MOVE (Ir.TEMP(r), Ir.MEM(Ir.BINOP(Ir.PLUS, Ir.CONST(n), ir_lhs))) ->
    (* move from memory to register *)
    let lhs = munch_exp ir_lhs in
    OP(sprintf "movl %d('s0), 'd0" n, [r], [lhs], None) |> emit

  | Ir.MOVE (Ir.TEMP(r), Ir.BINOP(op, Ir.TEMP(r1), Ir.CONST(n))) when r = r1 ->
    (* arithmetic *)
    emit(OP(sprintf "%s $%d, 'd0" (binop_to_instr op) n,
            [r], [r], None))

  | Ir.MOVE (Ir.TEMP(t), Ir.CONST(n)) ->
    emit(OP(sprintf "movl $%d, 'd0" n, [t], [], None))

  | Ir.MOVE (Ir.TEMP(d), Ir.TEMP(s)) ->
    emit(MOVE("movl 's0, 'd0", d, s))

  | Ir.MOVE (Ir.MEM(e), e1) ->
    let src = munch_exp e in
    let moveto = munch_exp e1 in
    (* dst is (), because it is the memory not the reg that holds the value *)
    OP("movl 's0, ('s1)", [], [moveto; src], None)
    |> emit
  | Ir.MOVE (Ir.TEMP(t), e) ->
    let src = munch_exp e in
    MOVE("movl 's0, 'd0", t, src)
    |> emit
  | Ir.EXP(e) ->
    let src = munch_exp e in
    MOVE("xor 's0, 'd0", src, src)
    |> emit
  | Ir.JUMP (Ir.NAME(l), ls) ->
    OP("jmp " ^ (assembly_label_string l), [], [], Some ls)
    |> emit

  | Ir.CJUMP (relop, e1, Ir.CONST(n), t, f) ->
    let r = munch_exp e1 in
    OP(sprintf "cmp $%d, 's0" n, [], [r], None) |> emit;
    OP(sprintf "%s %s" (relop_to_instr relop)
         (assembly_label_string t), [], [], Some([t; f])) |> emit

  | Ir.CJUMP (relop, e0, e1, t, f) ->
    let t0 = munch_exp e0 in
    let t1 = munch_exp e1 in
    OP("cmp 's0, 's1",
       [], (*NOTE: what is the out register? icc or xcc.
             but we won't use them in register allocation, so ignore it. *)
       (* Caveat: it is easy to write [t0; t1] here. *)
       [t1; t0], None)
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

let select_instr frame ir =
  instr_list := [];
  munch_stmt ir;
  List.rev !instr_list
