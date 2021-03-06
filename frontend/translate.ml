open Symbol
open Batteries

(* Notes on static link: static link (sl) is added during the
    translation.

    Functions level stored in environment will include the static link
    which is added during the translation of a function declaration by
    calling new_level, while function signature does not include the
    static link.

    When translate a call site, how can we tell whether the callee is
    a built-in function or a tiger function, or a tiger function that
    overwrites a built-in one? First all of, type checker will rejects
    an arity mismatched function call, which leaves us only
    semantics-correct calls. Recall the frame stores static link for
    tiger functions. We only need to compare the number of arguments
    recorded in the callee's frame (static link included if any) and
    that of its actual arguments (static link also included if
    any). The callee is a tiger function if the number does not match,
    an external function if match.

*)

type level = {
  parent : level option;
  frame : Arch.frame;
  cmp : int
}
(** level is a wrapper of Frame with additional _static_ scope
 * information *)

type access = level * Arch.access
(** access is a wrapper to Frame.access with additional level
 * information *)

type exp =
  | Ex of Ir.exp
  | Nx of Ir.stmt
  | Cx of (Temp.label -> Temp.label -> Ir.stmt)

type frag = Arch.frag

let frag_to_string = Arch.frag_to_string

let level_to_doc lev =
  let open Pprint in
  let rec to_doc l =
  match l.parent with
  | None -> 0, text (Arch.frame_to_string l.frame)
  | Some (lev') ->
    let ident, doc = to_doc lev' in
    let ident' = ident + 4 in
    let doc' = doc <->
               nest ident'
                 (line <-> text (Arch.frame_to_string l.frame)) in
    ident', doc'
  in
  let _, doc = to_doc lev in
  doc


let compare (a : level) (b : level) = compare a.cmp b.cmp

(** uniq is for compare levels *)
let uniq = ref 0

(** frag_list stores seen functions and string literals *)
let frag_list : Arch.frag list ref = ref []

let make_true_label () = Temp.new_label ~prefix:"true" ()
let make_false_label () = Temp.new_label ~prefix:"false" ()
let make_fi_label () = Temp.new_label ~prefix:"fi" ()

let debug_print () =
  print_endline "== debug Translate Fragment ==";
  print_endline ("total fragments: " ^ (string_of_int (List.length !frag_list)));
  List.iter (fun frag -> frag_to_string frag |> print_endline)
    !frag_list;
  print_endline "== debug end =="

(** To use an IR as an Ex, call this function *)
let rec unEx (exp : exp) : Ir.exp = match exp with
  | Ex (e) -> e
  | Nx (stmt) -> Ir.ESEQ(stmt, Ir.CONST(0))
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
  | Ex (e) -> begin match e with
      | Ir.ESEQ (s, e0) -> Ir.SEQ(s, Ir.EXP(e0))
      | _ -> Ir.EXP(e)
    end
  | Cx (genjump) ->
    (* compare expression is used as a stmt. To preserve potential
       side effect, place label after the stmt. *)
    let label_t, label_f = make_true_label(), make_false_label() in
    Ir.SEQ(genjump label_t label_f,
           Ir.SEQ(Ir.LABEL(label_f),
                  Ir.LABEL(label_t)))

(** To use an IR as a Cx, call this function *)
let unCx e : Temp.label -> Temp.label -> Ir.stmt = match e with
  | Cx (genjump) -> genjump
  | Ex (e) -> fun t f -> Ir.CJUMP(Ir.NE, e, Ir.CONST(0),
                                  t, f)
  | Nx (e) -> failwith ("type checker failed on: " ^ (Ir.stmt_to_string e))

let outermost = { parent = None;
                  frame = Arch.new_frame (Temp.named_label "tigermain") [true];
                  cmp = !uniq
                }

(** new_level returns a new level. It accepts an optional option
    add_static_link, default to true, for two kinds of `level`:
    external function must confine to C lang's calling convention, so
    a new_level must not implicitly add a static link; while tiger's
    function will always accept an extra static link. *)
let new_level ?add_static_link:(add=true) parent label formals : level =
  (* add static link *)
  let fm = if add then
      Arch.new_frame label (true :: formals)
    else
      Arch.new_frame label formals
  in
  incr uniq;
  { parent = Some parent; frame = fm; cmp = !uniq }

(** get_formals will return the formal arguments of a
    function. (static link is implemented as an argument but not
    included.)*)
let get_formals level : access list =
  let fm_formals = Arch.get_formals level.frame in
  (*level_to_doc level |> Pprint.print_doc_endline;*)
  match List.map (fun f -> level, f) fm_formals with
  | [] -> failwith "A level's formals cannot be empty list"
  | hd :: tl -> tl

let get_label level =
  Arch.get_name level.frame

let alloc_local level escape : access =
  let fm = level.frame in
  let fm_access = Arch.alloc_local fm escape in
  level, fm_access

let get_static_link level : Arch.access =
  let fm_formals = Arch.get_formals level.frame in
  List.hd fm_formals

(** This function add label to the given [stmt]*)
let wrap_label (l : Temp.label) (stmt : Ir.stmt) : Ir.stmt =
  Ir.SEQ(Ir.LABEL(l), stmt)

(** The following functions provides interface to create [exp] from
    source language *)

let const (i : int) : exp = Ex(Ir.CONST(i))

(** this function has side effect. It modifies the global variable frag_list *)
let string (s : string) : exp =
  let label = Temp.new_label ~prefix:"str" () in
  let frag = Arch.STRING(label, s) in
  frag_list := frag :: !frag_list;
  Ex(Ir.NAME(label))

(** FIXME *)
let nil () : exp = Ex(Ir.CONST(0))

let break lab : exp = Nx(Ir.JUMP(Ir.NAME(lab), [lab]))

let no_value () : exp = Nx(Ir.EXP(Ir.CONST(0)))

(** Note: simply recursive does not work. Bug occurs for simple case:

    let var c := 4 function g(a:int):int = c in
    g(1)
    end

    variable generates: (mem + (mem (+ fp InMEM(offset_c)))
    offset_arg0). example: sparc: (mem + (mem (+ fp 2039)) 2175), 2039
    is the first local var, 2175 is the first argment. The correct one is
    (mem + (mem (+ fp 2175)) 2039)

*)
let simple_var (acc : access) (use_level : level) : exp =
  let def_level, fm_acc = acc in
  (* check_level is current following-up level, fp_exp is an
     expression that describes fp from use_level to check_level. *)
  let rec get_var (check_level : level) (fp_exp : Ir.exp) : Ir.exp =
    if check_level = def_level then
      let ir = Arch.get_access_exp fp_exp fm_acc in
      ir
    else
      match check_level.parent with
      | None -> failwith "Undefined Variable. Type Checker has bugs."
      | Some (parent) ->
        (* get static link *)
        let sl : Arch.access = get_static_link check_level in
        (* follow up to find the def_level *)
        let fp_exp' = Arch.get_access_exp fp_exp sl in
        get_var parent fp_exp'
  in
  Ex(get_var use_level (Ir.TEMP(Arch.fp)))

let var_field (exp : exp) (fld : Symbol.t) fld_list : (exp * 'a) option =
  let rec find_rec fld fld_lst offset = match fld_lst with
    | [] -> None, offset
    | (sym, t) :: tl ->
      if sym = fld then
        Some (sym, t), offset
      else
        find_rec fld tl (offset + Arch.word_size)
  in
  match find_rec fld fld_list 0 with
  | None, _ -> None
  | Some (sym, t), offset ->
    let ir = Ir.MEM(Ir.BINOP(Ir.PLUS, unEx exp, Ir.CONST(offset))) in
    Some (Ex(ir), t)


let var_subscript base idx : exp =
  let idx' = Ir.BINOP(Ir.MUL, unEx idx, Ir.CONST(Arch.word_size)) in
  Ex(Ir.MEM(Ir.BINOP(Ir.PLUS, unEx base, idx')))

let binop op operand1 operand2 =
  let rand1 = unEx operand1 in
  let rand2 = unEx operand2 in
  Ex(Ir.BINOP(op, rand1, rand2))

let relop op operand1 operand2 =
  let rand1 = unEx operand1 in
  let rand2 = unEx operand2 in
  Cx(fun t f -> Ir.CJUMP(op, rand1, rand2, t, f))

let string_cmp op rand1 rand2 =
  let cmp = Arch.external_call "stringEqual" [unEx rand1; unEx rand2] in
  let cmpwith = match op with
    | Ir.EQ -> Ir.CONST(1)
    | Ir.NE -> Ir.CONST(0)
    | _ -> failwith "type checker should only allows string cmp using EQ and NE"
  in
  Cx(fun t f -> Ir.CJUMP(op, cmp, cmpwith, t, f))

let assign (lhs : exp) (rhs : exp) : exp =
  let l = unEx lhs in
  let r = unEx rhs in
  Nx(Ir.MOVE(l, r))

(** call's helper function. Given a function's own level (def_level,
    got from venv), and where it is used (the use_level, got when
    translation encounters a call), This function calculates an exp to
    describe def_level's statically enclosing function's
    fp. Conceptually, this one is the same as simple_var as they both
    follow up the static link, but this one is more complicated as we
    need to handle more cases.

    (Indentation means new level. `T` means toplevel -- outermost
    level that is. arg0 is the argument position for static link. )

    Case 1. use_level's parent = def_level's parent
    T
      foo (){}
      bar (){ foo() }

    Inside bar, bar's arg0 is foo's statically enclosing
    function's fp.

    Case 2. use_level = def_level
    T
      foo () { foo() }

    Inside foo, foo's arg0 is foo's statically enclosing
    function's fp.

    Case 3.
    T
      foo () {}
      ...
      foo();  // use foo in T

    Inside T, T's current fp is foo's statically enclosing function's
    fp.

    Case 4.
    T
      f0 () {}
      f1 () {
         f2 () {
           f3 () {
              f4 () {
                  f0();
      ...

    We need pass f0's statically enclosing function's fp (I name it
    Tlevel_fp) to f0.

    Tlevel_fp = f1's arg0
              = f2's arg0 + offset of f1's arg0
              = f3's arg0 + offset of f2's arg0 + offset of f1's arg0
              = ...

    To see this in a glance: f2's arg0 is f1's fp, f3's arg0 is f2's
    fp. When f0 is called, what we have is f4's arg0, and what we want
    is f1's arg0.
*)
let rec get_enclosing_level_fp def_level use_level : exp =

  let rec get_fp (check_level : level) (fp_exp : Ir.exp) : Ir.exp =
    if def_level = check_level then
      let sl : Arch.access = get_static_link check_level in
      Arch.get_access_exp fp_exp sl
    else
      match def_level.parent, check_level.parent with
      | Some (def_parent), _ when def_parent = check_level ->
        fp_exp
      | Some (def_parent), Some (check_parent) ->
        let sl : Arch.access = get_static_link check_level in
        let fp_exp' = Arch.get_access_exp fp_exp sl in
        get_fp  check_parent fp_exp'
      | _ -> failwith "static link not found"
  in
  Ex(get_fp use_level (Ir.TEMP(Arch.fp)))


(** def_level is the callee's own level. use_level is calling
    function's level. call will calculate the static link. *)
let call def_level use_level args : exp =
  let label = Arch.get_name def_level.frame in
  let args_ir = List.map (fun arg -> unEx arg) args in
  if (List.length (Arch.get_formals def_level.frame)) - 1
     = List.length args_ir then
    (* This is a tiger function *)
    let sl_exp = get_enclosing_level_fp def_level use_level in
    Ex(Ir.CALL(Ir.NAME(label), unEx sl_exp :: args_ir))
  else
    Ex(Ir.CALL(Ir.NAME(label), args_ir))


(** FIXME:
    1. is empty fields allowed? NO
    2. "malloc" should not be hard coded here. *)
let record (fields : exp list) =
  let temp = Ir.TEMP(Temp.new_temp()) in
  let n = List.length fields in
  let init_ir_stmts =
    List.mapi (fun i e ->
        let v = unEx e in
        Ir.MOVE(Ir.MEM(Ir.BINOP(Ir.PLUS,
                                Ir.CONST(i * Arch.word_size),
                                temp)),
                v)
      ) fields in
  let init_ir_stmt = Ir.seq init_ir_stmts in
  let alloca = Ir.MOVE(temp, Arch.external_call "allocRecord" [Ir.CONST(Arch.word_size * n)]) in
  Ex(Ir.ESEQ(Ir.SEQ(alloca, init_ir_stmt),
             temp))

let array (n : exp) (init : exp) : exp =
  let ir = Arch.external_call "initArray" [unEx n; unEx init] in
  Ex(ir)

(** construct Ir.SEQ from the given list except for the last one.
 * In general, given a list of expression [exp_lst],
 * this function constructs ESEQ(SEQ(SEQ(SEQ(e0 e1), e2),..), en),
 * and returns the associated value (typed ['a]) of en
*)
let seq (exp_list :  (exp * 'a) list) : exp * 'a =
  (* For example: seq 1 2 3 4 5
   * is converted to reduce 1 2 [3 4 5] *)
  let rec reduce init prev rest =
    let init', _ = init in
    let prev', prev_t = prev in
    match rest with
    | [] -> Ex(Ir.ESEQ (unNx init', unEx prev')), prev_t
    | hd :: tl ->
      let ir = Ir.SEQ (unNx init', unNx prev') in
      reduce (Nx ir, prev_t (*prev_t will be discarded*)) hd tl
  in
  match exp_list with
  | [] -> failwith "Translate.seq takes at least one argument"
  | hd :: [] -> hd
  | hd0 :: hd1 :: tl  -> reduce hd0 hd1 tl

let if_cond_unit_body tst thn (els : exp option) : exp =
  let label_t = make_true_label () in
  let label_f = make_false_label () in
  let label_fi = make_fi_label () in
  let tst_ir = (unCx tst) label_t label_f in
  let true_part =
    Ir.SEQ(unNx thn, Ir.JUMP(Ir.NAME(label_fi), [label_fi]))
    |> wrap_label label_t in
  let res = match els with
    | None ->
      Ir.SEQ(tst_ir,
             Ir.SEQ(true_part,
                    Ir.SEQ(Ir.LABEL(label_f), Ir.LABEL(label_fi))))
    | Some (e) ->
      let els_ir = unNx e in
      Ir.SEQ(tst_ir,
             Ir.SEQ(true_part,
                    Ir.SEQ(wrap_label label_f els_ir,
                           Ir.LABEL(label_fi)))) in
  Nx(res)

let if_cond_nonunit_body tst thn (els : exp option) : exp =
  let label_t = make_true_label () in
  let label_f = make_false_label () in
  let label_fi = make_fi_label () in
  let temp = Temp.new_temp () in
  let tst_ir = (unCx tst) label_t label_f in
  let true_part =
    Ir.SEQ(Ir.MOVE(Ir.TEMP(temp), unEx thn),
           Ir.JUMP(Ir.NAME(label_fi), [label_fi]))
    |> wrap_label label_t
  in
  let res = match els with
    | None -> failwith "use if_cond_unit_body to translate!"
    | Some (e) ->
      let els_part = Ir.MOVE(Ir.TEMP(temp), unEx e)
                     |> wrap_label label_f in
      Ir.ESEQ(Ir.SEQ(tst_ir,
                     Ir.SEQ(true_part,
                            Ir.SEQ(els_part, Ir.LABEL(label_fi)))),
              Ir.TEMP(temp))in
  Ex(res)

(**
 * tst_label:
 * if tst then
 *   (body; jump tst_label)
 * done_label:
*)
let while_loop tst body label_done: exp =
  let label_tst = Temp.new_label ~prefix:"while_test" () in
  let label_body = Temp.new_label ~prefix:"while_body" () in
  let tst_ir = (unCx tst) label_body label_done in
  let body_ir = Ir.SEQ(unNx body,
                       Ir.JUMP(Ir.NAME(label_tst), [label_tst]))
                |> wrap_label label_body in
  let res = Ir.SEQ(tst_ir,
                   Ir.SEQ(body_ir,
                          Ir.LABEL(label_done)))
            |> wrap_label label_tst in
  Nx(res)

let let_body exp_lst exp : exp =
  let ir_lst = List.map (fun e -> unNx e) exp_lst in
  Ex(Ir.ESEQ(Ir.seq ir_lst, unEx exp))

let proc_entry_exit ?(is_procedure=false) level fbody : unit =
  let fm = level.frame in
  (* Front-end here must know which machine register is used for
     returning value. *)
  let body = if is_procedure then
      Ir.SEQ(unNx fbody,
             Ir.MOVE(Ir.TEMP(Arch.rv), Ir.CONST(0)))
    else
      Ir.MOVE(Ir.TEMP(Arch.rv), unEx fbody) in
  (* do view shift *)
  let stmt = Arch.view_shift fm body in
  frag_list := Arch.PROC(stmt, fm) :: !frag_list

let get_result () : frag list =
  !frag_list
