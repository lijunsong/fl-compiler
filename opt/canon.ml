(** Rearrange IR to Cannoical IR.
    1. Removing ESEQ and SEQ;
    2. Raise CALL expression to a MOVE or EXP statement.
 *)
open Batteries

let unreachable () = failwith "unreachable"

let rec visit_stmt = function
  | Ir.MOVE(Ir.TEMP(t), Ir.CALL(f, args)) ->
     reorder_stmt (f :: args)
                  (function
                   | f' :: args' ->
                      assert(List.length args = List.length args');
                      Ir.MOVE(Ir.TEMP(t), Ir.CALL(f', args'))
                   | _ -> unreachable())
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

and visit_exp e : Ir.stmt * Ir.exp =
  (* Printf.printf "visit_exp: %s\n%!" (Ir.exp_to_string e); *)
  match e with
  | Ir.CONST (_) | Ir.NAME (_) | Ir.TEMP(_) ->
     Ir.EXP(Ir.CONST(0)), e
  | Ir.BINOP (op, e1, e2) ->
     reorder_exp [e1; e2] (function
                           | [e1'; e2'] -> Ir.BINOP(op, e1', e2')
                           | _ -> unreachable ())
  | Ir.MEM (e1) ->
     reorder_exp [e1] (function
                       | [e1'] -> Ir.MEM(e1')
                       | _ -> unreachable())
  | Ir.CALL (f, args) ->
     let temp = Ir.TEMP(Temp.new_temp()) in
     let ir = Ir.ESEQ(Ir.MOVE(temp, e), temp) in
     visit_exp ir
  | Ir.ESEQ (s0, e) ->
     let new_s0 = visit_stmt s0 in
     let new_s1, new_e = visit_exp e in
     Ir.SEQ(new_s0, new_s1), new_e

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


let linearize stmt : Ir.stmt list =
  let rec linear seq res = match seq with
    | Ir.SEQ(s1, s2) -> linear s1 (linear s2 res)
    | _ -> seq :: res
  in
  linear (visit_stmt stmt) []

let basic_blocks linearized =
  (* stmt_list: is current unprocessed stmt sequence.
   * curr_block_rev: is current basic block; stores a list of stmt in
   *                reversed order.
   * result_rev: stores a list of basic blocks in reversed order
   *
   * NOTE: reversing the order to increasing the performance.
   *)
  let rec split stmt_list (curr_block_rev : Ir.stmt list) result_rev =
    match stmt_list with
    | [] ->
       let result' = if curr_block_rev = [] then
                       result_rev
                     else
                       (List.rev curr_block_rev) :: result_rev
       in
       List.rev result'
    | stmt :: rest ->
       begin match stmt with
       | Ir.JUMP(_) | Ir.CJUMP(_) ->
          (* A jump starts a new block. *)
          let curr_block = List.rev (stmt :: curr_block_rev) in
          split rest [] (curr_block :: result_rev)
       | Ir.LABEL(label) ->
          (* A label ends a block. If previous block does not
           * end with JUMP/CJUMP, append JUMP to previous block *)
          let not_jump = function
            | Ir.JUMP(_) -> false
            | Ir.CJUMP(_) -> false
            | _ -> true in
          (* append a jump to prev block if necessary *)
          let append_jump =
            if List.length curr_block_rev = 0 ||
                 not_jump (List.hd curr_block_rev) then
              Ir.JUMP(Ir.NAME(label), [label]) :: curr_block_rev
            else
              curr_block_rev
          in
          let prev_block = List.rev append_jump in
          split rest [stmt] (prev_block :: result_rev)
       | _ ->
          split rest (stmt :: curr_block_rev) result_rev
       end
  in
  split linearized [] [], Temp.new_label ~prefix:"done" ()
