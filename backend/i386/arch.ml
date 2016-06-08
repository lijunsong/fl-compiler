(** This is activation record implementation of i386 archtecture. This implementation

    - Follows cdecl calling convention, which implies that
    - eax, ecx, edx are caller saved registers, and the rest are callee-saved
    - arguments are passed on stack
    - return value is passed in eax
    - caller cleans up the stack
    - labels are prefixed an underscore

    Here we say Register as physical machine register, and Temp as
    temporary values.
*)

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
  "%ebp"; "%esp"
]
(** all registers that are available for register allocation *)

(** registers that caller are responsible to save *)
let caller_save = [
  "%eax"; "%ecx"; "%edx"
]

(** registers that calee are responsible to save if to use it *)
let callee_save = [
  "%ebx"; "%esi"; "%edi"
]

(* maps from register to temp *)
module Register_map = Map.Make(String)

let reg_map = List.map
    (fun reg -> reg, Temp.new_temp()) registers
                   |> List.enum
                   |> Register_map.of_enum

let temp_map = Register_map.enum reg_map
                 |> Enum.map (fun (a,b)->b,a)
                 |> Temp.TempMap.of_enum

let temp_of_register (name : register) =
  try
    Register_map.find name reg_map
  with _ -> failwith (name ^ " Not found")

let register_of_temp (temp : Temp.temp) =
  try
    Some (Temp.TempMap.find temp temp_map)
  with
  | _ -> None

(** frame pointer *)
let fp = temp_of_register "%ebp"
(** return value *)
let rv = temp_of_register "%eax"
(** stack pointer *)
let sp = temp_of_register "%esp"

(** formals startat ebp+8, growing upward.*)
let new_frame (name : Temp.label) (formals : bool list) : frame =
  { name;
    formals = List.mapi (fun i f ->
        if f then
          InMem(word_size*i + 8)
        else
          InReg(Temp.new_temp())) formals;
    locals = []}

let get_name (fm : frame) = fm.name
let get_formals (fm : frame) = fm.formals

(** This function follows the calling convention to produce a
    label that follows the calling convention. *)
let label_to_string l : string =
  "_" ^ (Temp.label_to_string l)

(** locals are indexed based on fp. *)
let alloc_local fm escape =
  let len = List.length fm.locals in
  let loc = InMem((-word_size) * (len+1)) in
  fm.locals <- loc :: fm.locals;
  loc

let bias = 0

(** Given an expression for the base of an frame and given an access,
    return an expression for the in memory value of that access. *)
let get_access_exp (frame_base : Ir.exp) (acc : access) : Ir.exp = match acc with
  | InReg(temp) -> Ir.TEMP(temp)
  | InMem(offset) ->
    Ir.MEM(Ir.BINOP(Ir.PLUS, frame_base, Ir.CONST(offset)))

(** Given #formal and #locals, this function calculates stack size. 16
    bytes aligned. cdecl doesn't count formals as part of its stack
    size. For prologue use, the total subtract size must include
    additional 2 slots: return value and return address. *)
let get_stack_size localn =
  (localn/4+1) * 16 - 8

let rec maximum_callee_args stmt =
  let open Ir in
  let rec search = function
    | CONST(_)
    | NAME(_)
    | TEMP(_) -> 0
    | BINOP(_, l, r) ->
      max (search l) (search r)
    | MEM (e) -> search e
    | CALL (e, es) ->
      let m = List.length es in
      let len = m :: (search e) :: List.map search es in
      List.reduce max len
    | ESEQ (s, e) ->
      max (maximum_callee_args s) (search e)
  in
  List.reduce max (List.map search (children stmt))

(** Implement view shift on IR level.

    - Since we don't pass parameter in registers, so new_frame does
      nothing and view_shift does nothing for register parameters.

    - Save and Store callee-saved registers (that are used in stmt),
      so view shift generate move instructions to save those registers
      to maximize available registers. This cost will be removed when
      spilling is implemented.

    - Because of i386 convention, the real view shift, that is push
      ebp and move esp to ebp, is implemented as a hard-coded prolog
      and eiplog at assembly level. See proc_entry_exit2

    - Clean up locals are implemented here.
*)
let view_shift fm body =
  (* generate memory local and temp tuple for saving and restoring
     callee-saved registers *)
  let ebp = Ir.TEMP(fp) in
  let esp = Ir.TEMP(sp) in
  let temps = List.map (fun reg ->
      let acc = alloc_local fm true in
      let mem = get_access_exp ebp acc in
      let temp = temp_of_register reg in
      mem, Ir.TEMP(temp)) callee_save in
  (* generate view shift IR to update ebp and esp *)
  let stack_size = get_stack_size
      ((maximum_callee_args body) + (List.length fm.locals)) in
  let alloc_locals = Ir.MOVE(esp,
                            Ir.BINOP(Ir.MINUS, esp, Ir.CONST(stack_size))) in
  let save = List.map (fun (mem, temp) -> Ir.MOVE(mem, temp)) temps
             |> Ir.seq in
  let restore = List.map (fun (mem, temp) -> Ir.MOVE(temp, mem)) temps
                |> Ir.seq in
  let unwind_locals = Ir.MOVE(esp,
                         Ir.BINOP(Ir.PLUS, esp, Ir.CONST(stack_size))) in
  Ir.seq [alloc_locals; save; body; restore; unwind_locals]

let proc_entry_exit2 f instrs =
  let live_reg = List.map temp_of_register ["%eax"] in
  instrs @ [Assem.OP("", [], live_reg, None)]

(** For i386, it is easier to implement the real view shift in
    assembly lang, this function is implemented to generate view shift
    instruction too. *)
let add_prolog_epilog f body =
  (* caveat here: i386 uses _FOO as function foo's name. *)
  let f_name = label_to_string (get_name f) in
  let prolog = [
    ".global " ^ f_name;  (* TODO: not all functions are global*)
    f_name ^ ":";         (* function start *)
    "push %ebp";
    "movl %esp, %ebp";
  ] in
  let epil = [
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
  let l_str = label_to_string l in
  let str = [
    l_str ^ ":";
    sprintf ".long %d" (String.length s);
    sprintf ".asciz \"%s\"" s;
  ] in
  String.concat "\n" str
