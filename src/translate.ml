open Symbol
open Batteries

module type Frame = sig
  type frame
  type access

  (** [new_frame name formals] create a frame named l. A list of
  bool indicates whether each formal argument escapes. *)
  val new_frame : Temp.label -> bool list -> frame

  (** retrieve the given frame's name *)
  val get_name : frame -> Temp.label

  (** retrieve the given frame's formal arguments.  *)
  val get_formals : frame -> access list

  (** [alloc_local f escape] allocate a local variable on frame [f] with
  [escape] indicating whether the variable escapes *)
  val alloc_local : frame -> bool -> access

  val fp : Temp.temp ref

  (** the size of a word in a Frame *)
  val word_size : int

  (** [get_exp base access] given the base location of the access,
  this function returns the IR representing that location's content *)
  val get_exp : Ir.exp -> access -> Ir.exp
end

module SparcFrame : Frame = struct
  type access =
    | InReg of Temp.temp   (** which register to store *)
    | InMem of int         (** offset in the frame *)

  type frame = {
      name : Temp.label;
      formals : access list;
      mutable locals : access list;
    }

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

  let fp = ref (Temp.new_temp())

  let word_size = 4

  (** Given an expression for the base of an frame and given the
  access of that frame, return an expression for the memory. *)
  let get_exp (frame_base : Ir.exp) (acc : access) : Ir.exp = match acc with
    | InReg(temp) -> Ir.TEMP(temp)
    | InMem(offset) ->
       Ir.MEM(Ir.BINOP(Ir.PLUS, frame_base, Ir.CONST(offset)))
end

module F = SparcFrame

type level = { parent : level option; frame : F.frame; cmp : int }
(** level is a wrapper of Frame with additional _static_ scope
 * information *)

type access = level * F.access
(** access is a wrapper to Frame.access with additional level
 * information *)

type exp =
  | Ex of Ir.exp
  | Nx of Ir.stmt
  | Cx of (Temp.label -> Temp.label -> Ir.stmt)

let compare (a : level) (b : level) = compare a.cmp b.cmp

(** uniq is for compare levels *)
let uniq = ref 0

(*let dummy_exp = Ex (Ir.CONST(0))*)

let make_true_label () = Temp.new_label ~prefix:"true" ()
let make_false_label () = Temp.new_label ~prefix:"false" ()
(** To use an IR as an Ex, call this function *)
let unEx (exp : exp) : Ir.exp = match exp with
  | Ex (e) -> e
  | Nx (stmt) -> failwith "unEx(Nx)"
  | Cx (genjump) ->
     let label_t  = make_true_label () in
     let label_f =  make_false_label () in
     let res = Ir.TEMP (Temp.new_temp()) in
     Ir.ESEQ(Ir.SEQ(Ir.MOVE(res, Ir.CONST(1)),
                    Ir.SEQ(genjump label_t label_f,
                           Ir.SEQ(Ir.LABEL(label_f),
                                  Ir.SEQ(Ir.MOVE(res, Ir.CONST(0)),
                                         Ir.LABEL(label_t))))),
             res)

(** To use an IR as an Nx, call this function *)
let unNx = function
  | Nx (stmt) -> stmt
  | Ex (e) -> Ir.EXP(e)
  | Cx (genjump) ->
     let label_t, label_f = make_true_label(), make_false_label() in
     genjump label_t label_f

(** To use an IR as a Cx, call this function *)
let unCx e : Temp.label -> Temp.label -> Ir.stmt = match e with
  | Cx (genjump) -> genjump
  | Ex (e) -> failwith "NYI unCx"
  | Nx (e) -> failwith "unreachable"

let outermost = { parent = None;
                  frame = F.new_frame (Temp.new_label ~prefix:"main" ()) [];
                  cmp = !uniq
                }

let new_level parent label formals =
  let fm = F.new_frame label (true :: formals) in
  incr uniq;
  { parent = Some parent; frame = fm; cmp = !uniq }

(** get_formals will return the formal arguments of a
  function. (static link is implemented as an argument but not
  included.)*)
let get_formals level : access list =
  let fm_formals = F.get_formals level.frame in
  match List.map (fun f -> level, f) fm_formals with
  | [] -> failwith "A level's formals cannot be empty list"
  | hd :: tl -> tl

let get_label level =
  F.get_name level.frame

let alloc_local level escape : access =
  let fm = level.frame in
  let fm_access = F.alloc_local fm escape in
  level, fm_access

let get_static_link level : F.access =
  let fm_formals = F.get_formals level.frame in
  List.hd fm_formals

(** The following functions provides interface to create [exp] from
    source language *)

let const (i : int) : exp = Ex(Ir.CONST(i))

(** FIXME *)
let string (s : string) : exp = Ex(Ir.CONST(0))

(** FIXME *)
let nil () : exp = Ex(Ir.CONST(0))

(** FIXME: given a label, jump to label *)
let break () : exp = Ex(Ir.CONST(0))

let rec simple_var (acc : access) (use_level : level) : exp =
  let def_level, fm_acc = acc in
  if use_level = def_level then
    let ir = F.get_exp (Ir.TEMP(!F.fp)) fm_acc in
    Ex(ir)
  else
    (* get static link *)
    let sl : F.access = get_static_link use_level in
    (* follow up to find the def_level *)
    match use_level.parent with
    | None -> failwith "Undefined Variable. Type Checker has bugs."
    | Some (parent) ->
       let follow_up = unEx(simple_var acc parent) in
       let ir = F.get_exp follow_up sl in
       Ex(ir)

let var_field exp fld fld_list : (exp * 'a) option =
  let rec find_rec fld fld_lst offset = match fld_lst with
    | [] -> None, offset
    | (sym, t) :: tl ->
       if sym = fld then
         Some (sym, t), offset
       else
         find_rec fld tl (offset + F.word_size)
  in
  match find_rec fld fld_list 0 with
  | None, _ -> None
  | Some (sym, t), offset ->
     let ir = Ir.MEM(Ir.BINOP(Ir.PLUS, exp, Ir.CONST(offset))) in
     Some (Ex(ir), t)


let var_subscript base idx : exp =
  Ex(Ir.MEM(Ir.BINOP(Ir.PLUS, base, idx)))

let binop op operand1 operand2 =
  let rand1 = unEx operand1 in
  let rand2 = unEx operand2 in
  Ex(Ir.BINOP(op, rand1, rand2))

let relop op operand1 operand2 =
  let rand1 = unEx operand1 in
  let rand2 = unEx operand2 in
  Cx(fun t f -> Ir.CJUMP(op, rand1, rand2, t, f))

let assign (lhs : exp) (rhs : exp) : exp =
  let l = unEx lhs in
  let r = unEx rhs in
  Nx(Ir.MOVE(l, r))

(** FIXME: add static link to args_ir *)
let call def_level args : exp =
  let label = F.get_name def_level.frame in
  let args_ir = List.map (fun arg -> unEx arg) args in
  Ex(Ir.CALL(Ir.NAME(label), args_ir))

(** FIXME:
    1. is empty fields allowed?
    2. "malloc" should not be hard coded here. *)
let record (fields : exp list) =
  let temp = Ir.TEMP(Temp.new_temp()) in
  let n = List.length fields in
  let init_ir_stmts =
    List.mapi (fun i e ->
        let v = unEx e in
        Ir.MOVE(Ir.MEM(Ir.BINOP(Ir.PLUS,
                                Ir.CONST(i * F.word_size),
                                temp)),
                v)
      ) fields in
  let init_ir_stmt = Ir.seq init_ir_stmts in
  let alloca = Ir.MOVE(temp, Ir.CALL(Ir.NAME(Temp.named_label("malloc")),
                                     [Ir.CONST(F.word_size * n)])) in
  Ex(Ir.ESEQ(Ir.SEQ(alloca, init_ir_stmt),
             temp))

(** evaluate all except for the last one for side effect.
 * TODO: simplify this when you know what to do for empty sequence.*)
let seq exp_list : exp =
  let rec seq_rec (seqs : exp list) : Ir.exp = match seqs with
    | [] -> failwith "unreachable"
    | hd :: [] -> failwith "unreachable"
    | hd :: tl :: [] -> Ir.ESEQ(unNx hd, unEx tl)
    | hd :: tl -> begin match seq_rec tl with
                 | Ir.ESEQ (stmt, e) -> Ir.ESEQ(Ir.SEQ(unNx hd, stmt), e)
                 | _ -> failwith "unreachable"
                 end
  in
  match exp_list with
  | [] -> nil ()
  | hd :: [] -> hd
  | lst -> Ex(seq_rec lst)
