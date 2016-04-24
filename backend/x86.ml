open Frame
open Batteries
open Printf

(** This could be a functor. There is a lot of common code in x86 and
sparc *)
module X86Frame : Frame = struct
  type register = string with sexp

  type access =
    | InReg of Temp.temp
    | InMem of int
  with sexp

  type frame = {
      name : Temp.label;
      formals : access list;
      mutable locals : access list;
    } with sexp

  type frag =
    | PROC of Ir.stmt * frame
    | STRING of Temp.label * string
  with sexp

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
                      InReg(Temp.new_temp()));
      locals = []}

  let get_name (fm : frame) = fm.name
  let get_formals (fm : frame) = fm.formals

  (** locals are indexed based on fp. *)
  let alloc_local fm escape =
    let loc = InMem((-word_size) * !count_locals) in
    fm.locals <- loc :: fm.locals;
    incr count_locals;
    loc

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
                     | InReg(t) -> Ir.Temp(t)
                     | InMem(offset) as acc -> get_exp (Ir.TEMP(fp)) acc in
                   Ir.MOVE(Ir.TEMP(Temp.new_temp()), src))
                        fm.formals
    in
    Ir.SEQ(Ir.seq movs, stmt)


end
