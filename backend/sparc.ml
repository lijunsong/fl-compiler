open Frame
open Sexplib.Std
open Sexplib
open Batteries
open Printf

module SparcFrame : Frame = struct
  type register = string with sexp

  type access =
    | InReg of Temp.temp   (** which register to store *)
    | InMem of int         (** offset in the frame *)
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

  let word_size = 8

  let registers = [
    "%l0"; "%l1"; "%l2"; "%l3"; "%l4"; "%l5";
    "%o0"; "%o1"; "%o2"; "%o3"; "%o4"; "%o5";
    "%i0"; "%i1"; "%i2"; "%i3"; "%i4"; "%i5";
    "%g0"; "%g1"; "%g2"; "%g3"; "%g4"; "%g5";
    "%sp"; "%fp";
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

  let fp = get_temp "%fp"

  let rv = get_temp "%o0"

  (** The BIAS in Sparc frame. *)
  let bias = 2047

  (** count_locals initializes to 1, so that the first local variable
      has offset -word_size from fp *)
  let count_locals = ref 1

  (** formals on Sparc reside at fp + 128, growing upward. *)
  let new_frame (name : Temp.label) (formals : bool list) : frame =
    count_locals := 1;
    { name;
      formals = List.mapi (fun i f ->
                    if f then InMem(word_size * (i+16) + bias) (* FIXME *)
                    else let t = Temp.new_temp() in
                         InReg(t)) formals;
      locals = [];
    }

  let get_name (fm : frame) = fm.name
  let get_formals (fm : frame) = fm.formals

  let alloc_local fm escape =
    let loc = InMem((-word_size) * !count_locals + bias) in
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
      size. *)
  let get_stack_size formals locals =
    let formal_n = List.length formals in
    let minimal_size = 176 in
    let locals_n = List.length locals in
    let locals_add_over6formals = if formal_n > 6 then
        locals_n + formal_n - 6
      else
        locals_n in
    minimal_size + (locals_add_over6formals + 1) / 2 * 2 * 8

  (** implement view shift *)
  let proc_entry_exit1 f stmt = stmt

  let proc_entry_exit2 f instrs =
    (* TODO: list callee-saved registers *)
    let live_reg =
      List.map get_temp
               ["%i0"; "%i1"; "%i2"; "%i3"; "%i4"; "%i5"]
    in
    instrs @ [
        Assem.OP("", [], live_reg, None)
      ]

  let proc_entry_exit3 f body =
    let stack_size = get_stack_size f.formals f.locals in
    let f_name = get_name f |> Temp.label_to_string in
    let prolog = [
      ".global " ^ f_name;  (* TODO: not all functions are global*)
      f_name ^ ":";         (* function start *)
      sprintf "save %%sp, %d, %%sp" (-stack_size); (* register window shift*)
    ] in
    (* NOTE: remember epilog takes a delay-slot. *)
    let epil = [
      "return %i7 + 8";
      "restore %g0,%g0,%g0"; (* TODO: need to return value. *)
    ] in
    (prolog @ body) @ epil

  let external_call f args =
    Ir.CALL(Ir.NAME(Temp.named_label f), args)

  let debug_dump fm =
    Sexp.output_hum Pervasives.stdout (sexp_of_frame fm);
    print_string "\n"

  (** Local Labels on Sparc starts with ".". This function is
      duplicated in codegen *)
  let assembly_label_string l : string =
    "." ^ (Temp.label_to_string l)

  let string l s =
    let l_str = assembly_label_string l in
    let str = [
      l_str ^ ":";
      sprintf ".asciz %s" s;
      sprintf ".type %s,#object" l_str;
      sprintf ".size %s,%d" l_str ((String.length s) + 1);
    ] in
    String.concat "\n" str
end
