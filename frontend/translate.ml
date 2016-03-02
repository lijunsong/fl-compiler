open Sexplib.Std
open Sexplib
open Symbol
open Sparc
open Batteries

module F = SparcFrame

type level = { parent : level option; frame : F.frame; cmp : int } with sexp
(** level is a wrapper of Frame with additional _static_ scope
 * information *)

type access = level * F.access with sexp
(** access is a wrapper to Frame.access with additional level
 * information *)

type exp =
  | Ex of Ir.exp
  | Nx of Ir.stmt
  | Cx of (Temp.label -> Temp.label -> Ir.stmt) with sexp

type frag = F.frag with sexp

let compare (a : level) (b : level) = compare a.cmp b.cmp

(** uniq is for compare levels *)
let uniq = ref 0

(** frag_list stores seen functions and string literals *)
let frag_list : F.frag list ref = ref []

let make_true_label () = Temp.new_label ~prefix:"true" ()
let make_false_label () = Temp.new_label ~prefix:"false" ()
let make_fi_label () = Temp.new_label ~prefix:"fi" ()

let debug_print () =
  print_endline "== debug Translate Fragment ==";
  print_endline ("total fragments: " ^ (string_of_int (List.length !frag_list)));
  List.iter (fun frag ->
      Sexp.output_hum Pervasives.stdout (F.sexp_of_frag frag);
      print_endline "")
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
     let label_t, label_f = make_true_label(), make_false_label() in
     genjump label_t label_f

(** To use an IR as a Cx, call this function *)
let unCx e : Temp.label -> Temp.label -> Ir.stmt = match e with
  | Cx (genjump) -> genjump
  | Ex (e) -> fun t f -> Ir.CJUMP(Ir.NE, e, Ir.CONST(0),
                                  t, f)
  | Nx (e) -> failwith ("type checker failed on: " ^ (Ir.stmt_to_string e))

let outermost = { parent = None;
                  frame = F.new_frame (Temp.new_label ~prefix:"main" ()) [];
                  cmp = !uniq
                }

let new_level parent label formals : level =
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

(** This function add label to the given [stmt]*)
let wrap_label (l : Temp.label) (stmt : Ir.stmt) : Ir.stmt =
  Ir.SEQ(Ir.LABEL(l), stmt)

(** The following functions provides interface to create [exp] from
    source language *)

let const (i : int) : exp = Ex(Ir.CONST(i))

(** this function has side effect. It modifies the global variable frag_list *)
let string (s : string) : exp =
  let label = Temp.new_label ~prefix:"str" () in
  let frag = F.STRING(label, s) in
  frag_list := frag :: !frag_list;
  Ex(Ir.NAME(label))

(** FIXME *)
let nil () : exp = Ex(Ir.CONST(0))

(** FIXME: given a label, jump to label *)
let break () : exp = Ex(Ir.CONST(0))

let no_value () : exp = Nx(Ir.EXP(Ir.CONST(0)))

let rec simple_var (acc : access) (use_level : level) : exp =
  let def_level, fm_acc = acc in
  if use_level = def_level then
    let ir = F.get_exp (Ir.TEMP(F.fp)) fm_acc in
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

let var_field (exp : exp) (fld : Symbol.t) fld_list : (exp * 'a) option =
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
     let ir = Ir.MEM(Ir.BINOP(Ir.PLUS, unEx exp, Ir.CONST(offset))) in
     Some (Ex(ir), t)


let var_subscript base idx : exp =
  Ex(Ir.MEM(Ir.BINOP(Ir.PLUS, unEx base, unEx idx)))

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
    1. is empty fields allowed? NO
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
  let alloca = Ir.MOVE(temp, F.external_call "malloc" [Ir.CONST(F.word_size * n)]) in
  Ex(Ir.ESEQ(Ir.SEQ(alloca, init_ir_stmt),
             temp))

let array (n : exp) (init : exp) : exp =
  let ir = F.external_call "initArray" [unEx n; unEx init] in
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
 * else
 * done_label:
 *)
let while_loop tst body : exp =
  let label_tst = Temp.new_label ~prefix:"while_test" () in
  let label_body = Temp.new_label ~prefix:"while_body" () in
  let label_done = Temp.new_label ~prefix:"while_done" () in
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

let proc_entry_exit level fbody : unit =
  let fm = level.frame in
  let body = Ir.MOVE(Ir.TEMP(F.rv), unEx fbody) in
  let stmt = F.proc_entry_exit1 fm body in
  frag_list := F.PROC(stmt, fm) :: !frag_list

let get_result () : frag list =
  !frag_list
