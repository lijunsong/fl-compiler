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


let basic_blocks linearized =
  (* stmt_list: is current unprocessed stmt sequence.
   * curr_block_rev: is current basic block; stores a list of stmt in
   *                reversed order.
   * result_rev: stores a list of basic blocks in reversed order
   *
   * NOTE: reversing the order to increasing the performance.
  *)
  let done_label = Temp.new_label ~prefix:"exit" () in
  let is_jump = function
    | Ir.JUMP(_) | Ir.CJUMP(_) -> true
    | _ -> false in
  let rec split stmt_list (curr_block_rev : Ir.stmt list) result_rev =
    match stmt_list, curr_block_rev with
    | [], [] -> List.rev result_rev
    | [], lst -> List.rev ((List.rev curr_block_rev) :: result_rev)
    | Ir.LABEL(l) :: rest, [ ] ->
       split rest [Ir.LABEL(l)] result_rev
    | Ir.LABEL(l) :: rest, prev :: rest' when is_jump prev ->
       split rest [Ir.LABEL(l)] ((List.rev curr_block_rev) :: result_rev)
    | Ir.LABEL(l) :: rest, prev :: rest' ->
       let add_jump = Ir.JUMP(Ir.NAME(l), [l]) :: curr_block_rev in
       split rest [Ir.LABEL(l)] ((List.rev add_jump) :: result_rev)
    | stmt :: rest, lst when is_jump stmt ->
      let lst = if lst = [] then
          (*failwith ("basic_blocks: jump following an empty block. " ^
                   "Look like a bug in irgen and basic block.")*)
          [stmt; Ir.LABEL(Temp.new_label())]
        else
          stmt :: lst in
      let block = List.rev lst in
      split rest [] (block :: result_rev)
    | stmt :: rest, [ ] -> (* here stmt can not be a label *)
      split rest [stmt; Ir.LABEL(Temp.new_label())] result_rev
    | stmt :: rest, lst ->
       split rest (stmt :: lst) result_rev
  in
  let linearized' = linearized @ [Ir.JUMP(Ir.NAME(done_label), [done_label])] in
  split linearized' [] [], done_label

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

let trace_schedule (basic_blocks, lexit) : Ir.stmt list =
  (* [get_block_jump_info blocks] returns a hashmap mapping from
     labels to blocks that they belong to, and a list of blocks'
     starting label and jump-to labels information *)
  let get_all_jump_info bbs : (Ir.stmt list * bool) BlockMap.t =
    (* arg bbs is basic blocks *)
    let rec get_info_iter bbs map = match bbs with
      | [] -> map
      | bb :: rest ->
        let label, jump_to = get_jump_info bb in
        let map' = BlockMap.add label (bb, false) map in
        get_info_iter rest map'
    in
    get_info_iter bbs BlockMap.empty
  in
  let blockMap= get_all_jump_info basic_blocks in
  (* This function returns traced blocks. Blocks are not flattened yet. *)
  let rec trace_block cur_label bb_queue
      (cur_trace_rev : Ir.stmt list list) visited_info : Ir.stmt list list =
    let cur_block, visited = match get_basic_block cur_label visited_info with
      | None -> [], true
      | Some (b, v) -> b, v in
    let queue =
      if visited then bb_queue
      else
        let _, jumpto = get_jump_info cur_block in
        bb_queue @ jumpto
    in
    match queue, cur_block, visited with
    | [], _, true -> (* cur_label is visited already *)
      List.rev cur_trace_rev
    | [], block, false ->
      List.rev (block :: cur_trace_rev)
    | next :: tl, _, true -> (* cur_label is visited already *)
      trace_block next tl cur_trace_rev visited_info
    | next :: tl, block, false ->
      (* mark visited *)
      let visited_info' = BlockMap.add cur_label (block, true) visited_info in
      trace_block next tl (block :: cur_trace_rev) visited_info'
  in
  let label0, _ = get_jump_info (List.hd basic_blocks) in
  let res = trace_block label0 [] [] blockMap in
  (List.flatten res) @ [Ir.LABEL(lexit)]
