(** Rearrange IR to Cannoical IR.
    1. Removing ESEQ and SEQ;
    2. Raise CALL expression to a MOVE or EXP statement.
 *)
open Batteries

let unreachable () = failwith "unreachable"

(** [visit_stmt stmt] is the main function to call to generate
    canonical IR. *)
let rec visit_stmt = function
  | Ir.MOVE(Ir.TEMP(t), r) ->
     reorder_stmt [r]
                  (function
                   | [r'] -> Ir.MOVE(Ir.TEMP(t), r')
                   | _ -> unreachable() )
  | Ir.MOVE(l, r) ->
     reorder_stmt [l; r]
                  (function
                   | [l'; r'] -> Ir.MOVE(l', r')
                   | _ -> unreachable() )
  | Ir.EXP(e) ->
     reorder_stmt [e] (function
                       | [e'] -> Ir.EXP(e')
                       | _ -> unreachable ())
  | Ir.JUMP (e, list) ->

     reorder_stmt [e] (function
                       | [e'] -> Ir.JUMP(e', list)
                       | _ -> unreachable ())
  | Ir.CJUMP (op, e1, e2, l1, l2) ->
     reorder_stmt [e1; e2] (function
                            | [e1'; e2'] -> Ir.CJUMP(op, e1', e2', l1, l2)
                            | _ -> unreachable ())
  | Ir.SEQ (s1, s2) ->
     let s1' = visit_stmt s1 in
     let s2' = visit_stmt s2 in
     Ir.SEQ(s1', s2')
  | Ir.LABEL (_) as l -> l

(** [visit_exp exp] is a helper function, it extracts all sub-exp in
    the given exp that need to raise, and replaces these sub-exp with
    temps. *)
and visit_exp e : Ir.stmt * Ir.exp =
  (* Printf.printf "visit_exp: %s\n%!" (Ir.exp_to_string e); *)
  match e with
  | Ir.CONST (_) | Ir.NAME (_) | Ir.TEMP(_) ->
    Ir.EXP(Ir.CONST(0)), e
  | Ir.BINOP (op0, Ir.BINOP(op1, e1, e2), e3) ->
    (* raise the first BINOP for clarity *)
    let t = Ir.TEMP(Temp.new_temp()) in
    let move = Ir.MOVE(t, Ir.BINOP(op1, e1, e2)) in
    let others, exp = visit_exp (Ir.BINOP(op0, t, e3))in
    Ir.SEQ(move, others), exp
  | Ir.BINOP (op, e1, e2) ->
     reorder_exp [e1; e2] (function
                           | [e1'; e2'] -> Ir.BINOP(op, e1', e2')
                           | _ -> unreachable ())
  | Ir.MEM (e1) ->
     reorder_exp [e1] (function
                       | [e1'] -> Ir.MEM(e1')
                       | _ -> unreachable())
  | Ir.CALL (f, args) ->
    (* MOVE the result of CALL to a temp and use the temp to replace
       CALL. *)
    let prepend, new_call = reorder_exp (f :: args)
        (function
          | f' :: args' -> Ir.CALL(f', args')
          | _ -> unreachable())
    in
    let temp = Ir.TEMP(Temp.new_temp()) in
    Ir.SEQ(prepend,
           Ir.MOVE(temp, new_call)), temp
  | Ir.ESEQ (s0, e) ->
     let new_s0 = visit_stmt s0 in
     let new_s1, new_e = visit_exp e in
     Ir.SEQ(new_s0, new_s1), new_e

(** For a statement S, reorder_stmt takes the to-be-reordered children
    of S, and a function taking new children to replace the old
    children with new ones. It returns a new statement including

    1. raised statements
    2. new S with replaced children
*)
and reorder_stmt (children : Ir.exp list)
                 (replace : (Ir.exp list -> Ir.stmt)): Ir.stmt =
  let to_prepend, new_exp = List.map visit_exp children
                            |> List.split in
  let stmt = replace new_exp in
  Ir.SEQ(Ir.seq to_prepend, stmt)

and reorder_exp (children : Ir.exp list)
                (replace : (Ir.exp list -> Ir.exp)) : Ir.stmt * Ir.exp =
  let to_prepend, new_exp = List.map visit_exp children
                            |> List.split in
  let exp = replace new_exp in
  Ir.seq to_prepend, exp


(* drop EXP(CONST(0)). keep other EXP(CONST()) as tests uses a lot
EXP(CONST) *)
let drop_const_exp seq =
  List.filter(fun ir -> match ir with
                     | Ir.EXP(Ir.CONST(0)) -> false
                     | _ -> true) seq


(* Linearize drops SEQ *)
let linearize stmt : Ir.stmt list =
  let rec linear seq res = match seq with
    | Ir.SEQ(s1, s2) -> linear s1 (linear s2 res)
    | _ -> seq :: res
  in
  linear (visit_stmt stmt) []
  |> drop_const_exp


(** ------ Trace ----- *)

(** BlockMap maps from a label to the basic_block that it starts and a
    bool indicating "visited" *)
module BlockMap =  Temp.LabelMap

let get_basic_block label map =
  if BlockMap.mem label map then
    Some (BlockMap.find label map)
  else
    None

(** [get_jump_info one_block] returns the block's starting label and
    jump-to labels *)
let get_jump_info block : Temp.label * Temp.label list =
    let block_label = match List.hd block with
      | Ir.LABEL(l) -> l
      | ir ->
        failwith (Printf.sprintf
                    "non-label %s found at the start of a block"
                    (Ir.stmt_to_string ir))
    in
    let jump_to = match List.last block with
      | Ir.JUMP(_, labels) -> labels
      | Ir.CJUMP(_, _, _, t, f) -> [f; t]
      | _ -> [] (* failwith "non-jump found at the end of a block" *)
    in
    block_label, jump_to
