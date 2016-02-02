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
end

module F = SparcFrame

type level = { parent : level option; frame : F.frame }

type access = level * F.access

type exp =
  | Ex of Ir.exp
  | Nx of Ir.stmt
  | Cx of (Temp.label -> Temp.label -> Ir.stmt)

let const (i : int) = Ir.CONST(i)
let dummy_exp = Ex (const 0)

let make_true_label () = Temp.new_label ~prefix:"true" ()
let make_false_label () = Temp.new_label ~prefix:"false" ()

(** To use an IR as an Ex, call this function *)
let unEx (exp : exp) : Ir.exp = match exp with
  | Ex (e) -> e
  | Nx (stmt) -> failwith "unEx(Nx)"
  | Cx (genjump) ->
     let label_t  = Temp.new_label ~prefix:"true" () in
     let label_f = Temp.new_label ~prefix:"false" () in
     let res = Ir.TEMP (Temp.new_temp()) in
     Ir.ESEQ(Ir.SEQ(Ir.MOVE(res, const 1),
                    Ir.SEQ(genjump label_t label_f,
                           Ir.SEQ(Ir.LABEL(label_f),
                                  Ir.SEQ(Ir.MOVE(res, const 0),
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
                }

let new_level parent label formals =
  let fm = F.new_frame label (true :: formals) in
  { parent = Some parent; frame = fm }

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
