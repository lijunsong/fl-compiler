open Batteries
open Printf

type register = string

type access =
  | InReg of Temp.temp
  | InMem of int

type frame = {
  name : Temp.label;
  formals : access list;
  mutable locals : access list;
}

type frag =
  | PROC of Ir.stmt * frame
  | STRING of Temp.label * string

let access_to_string = function
  | InReg (t) -> "inreg " ^ (Temp.temp_to_string t)
  | InMem (n) -> "inmem " ^ (string_of_int n)

let frame_to_string f =
  Printf.sprintf "%s (%s) {local: %s}"
    (Temp.label_to_string f.name)
    (String.concat "," (List.map access_to_string f.formals))
    (String.concat "," (List.map access_to_string f.locals))

let frag_to_string = function
  | PROC (stmt, fm) -> (Ir.stmt_to_string stmt) ^ "\n" ^ (frame_to_string fm) ^ "\n"
  | STRING (l, s) -> (Temp.label_to_string l) ^ ": " ^ s

let word_size = 4

let registers = [
  "%eax"; "%ebx"; "%ecx"; "%edx"; "%esi"; "%edi";
  "%ebp"; "%esp";
]

(* maps from name to temp *)
module RegNameMap = Map.Make(String)
(* maps from temp to name*)
module RegMap = Temp.TempMap

let reg_name_map = List.map
    (fun reg -> reg, Temp.new_temp()) registers
                   |> List.enum
                   |> RegNameMap.of_enum

let known_temp = RegNameMap.enum reg_name_map
                 |> Enum.map (fun (a,b)->b,a)
                 |> RegMap.of_enum

let get_temp (name : register) =
  try
    RegNameMap.find name reg_name_map
  with _ -> failwith (name ^ " Not found")

let get_register_name (reg : Temp.temp) =
  try
    Some (RegMap.find reg known_temp)
  with
  | _ -> None

(** The ebp frame pointer *)
let fp = get_temp "%ebp"

let rv = get_temp "%eax"

(** init to 1. The first local resides at ebp-4 *)
let count_locals = ref 1

(** formals startat ebp+8, growing upward.*)
let new_frame (name : Temp.label) (formals : bool list) : frame =
  count_locals := 1;
  { name;
    formals = List.mapi (fun i f ->
        if f then
          InMem(word_size*i + 8)
        else
          InReg(Temp.new_temp())) formals;
    locals = []}

let get_name (fm : frame) = fm.name
let get_formals (fm : frame) = fm.formals

(** Local Labels on X86 starts with "_". This function is duplicated
    in codegen *)
let assembly_label_string l : string =
  "_" ^ (Temp.label_to_string l)

(** locals are indexed based on fp. *)
let alloc_local fm escape =
  let loc = InMem((-word_size) * !count_locals) in
  fm.locals <- loc :: fm.locals;
  incr count_locals;
  loc

let bias = 0

(** Given an expression for the base of an frame and given the
    access of that frame, return an expression for contents of the
    memory. *)
let get_exp (frame_base : Ir.exp) (acc : access) : Ir.exp = match acc with
  | InReg(temp) -> Ir.TEMP(temp)
  | InMem(offset) ->
    Ir.MEM(Ir.BINOP(Ir.PLUS, frame_base, Ir.CONST(offset)))

(** Given #formal and #locals, this function calculates stack
    size. 16 bytes aligned. x86 doesn't count formals as part of its
    stack size. For prologue use, the total subtract size must
    include additional 2 slots: return value and return address. *)
let get_stack_size formals locals =
  let local_n = List.length locals in
  (local_n/4+1) * 16

(** Implement view shift. x86 passes arguments on stack, so the view
    shift only load each argument into temporaries. *)
let proc_entry_exit1 fm stmt =
  let movs = List.map (fun acc ->
      let src = match acc with
        | InReg(t) -> Ir.TEMP(t)
        | InMem(offset) as acc -> get_exp (Ir.TEMP(fp)) acc in
      Ir.MOVE(Ir.TEMP(Temp.new_temp()), src))
      fm.formals
  in
  Ir.SEQ(Ir.seq movs, stmt)

let proc_entry_exit2 f instrs =
  let live_reg = List.map get_temp ["%ebx"; "%edi"; "%esi"] in
  instrs @ [Assem.OP("", [], live_reg, None)]

let proc_entry_exit3 f body =
  let stack_size = get_stack_size f.formals f.locals in
  (* caveat here: x86 uses _FOO as function foo's name. *)
  let f_name = get_name f |> Temp.label_to_string in
  let prolog = [
    ".global " ^ f_name;  (* TODO: not all functions are global*)
    f_name ^ ":";         (* function start *)
    "push %ebp";
    "movl %esp, %ebp";
    "pushl %edi";  (* TODO: push these two only when they are used. *)
    "pushl %esi";
    sprintf "subl $%d, %%esp" stack_size;
  ] in
  let epil = [
    sprintf "addl $%d, %%esp" stack_size;
    "popl %esi";
    "popl %edi";
    "popl %ebp";
    "retl";
  ] in
  (prolog @ body) @ epil

let external_call f args =
  Ir.CALL(Ir.NAME(Temp.named_label f), args)

let debug_dump fm =
  print_endline (frame_to_string fm)

(** The implementation of string is interesting. If runtime.c
    defines the length as a long long, we need a xword instead of a
    word here. As currently runtime.c defines length as an int, we
    just need a word to store its length.*)
let string l s =
  let l_str = assembly_label_string l in
  let str = [
    l_str ^ ":";
    sprintf ".word %d" (String.length s);
    sprintf ".ascii \"%s\"" s;
  ] in
  String.concat "\n" str
