open Frame
open Sexplib.Std
open Sexplib
open Batteries

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

  let registers = [
    "o0"; "o1"; "o2"; "o3"; "o4"; "o5"; "o6";
    "i0"; "i1"; "i2"; "i3"; "i4"; "i5"; "i6";
    "g0";
    "sp"; "fp"
  ]

  module RegMap = Map.Make(String)

  let reg_map = List.map
      (fun reg -> reg, Temp.new_temp()) registers
                |> List.enum
                |> RegMap.of_enum

  let get_register reg =
    RegMap.find reg reg_map

  let new_frame (name : Temp.label) (formals : bool list) : frame =
    { name;
      formals = List.mapi (fun i f ->
                    if f then InMem((-4) * i) (* FIXME *)
                    else let t = Temp.new_temp() in
                         InReg(t)) formals;
      locals = [];
    }

  let count_locals = ref 0
  let get_name (fm : frame) = fm.name
  let get_formals (fm : frame) = fm.formals

  let alloc_local fm escape =
    incr count_locals;
    let loc = InMem(4 * !count_locals) in
    fm.locals <- loc :: fm.locals;
    loc

  let fp = get_register "fp"

  let rv = get_register "o0"

  let word_size = 4

  (** Given an expression for the base of an frame and given the
  access of that frame, return an expression for contents of the
  memory. *)
  let get_exp (frame_base : Ir.exp) (acc : access) : Ir.exp = match acc with
    | InReg(temp) -> Ir.TEMP(temp)
    | InMem(offset) ->
       Ir.MEM(Ir.BINOP(Ir.PLUS, frame_base, Ir.CONST(offset)))

  (** FIXME *)
  let proc_entry_exit1 f stmt = stmt

  let proc_entry_exit2 f instrs =
    (* TODO: list callee-saved registers *)
    let live_reg =
      List.map get_register
               ["g0"; "o0"; "o1"; "o2"; "o3"; "o4"; "o5"]
    in
    instrs @ [
        Assem.OP("", [], live_reg, None)
      ]

  let proc_entry_exit3 f body =
    let prol = "procedure: " ^ (Temp.label_to_string f.name) in
    let epil = "end " ^ (Temp.label_to_string f.name) in
    (prol :: body) @ [epil]

  let external_call f args =
    Ir.CALL(Ir.NAME(Temp.named_label f), args)

  let debug_dump fm =
    Sexp.output_hum Pervasives.stdout (sexp_of_frame fm);
    print_string "\n"

end
