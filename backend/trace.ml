(** Description: This is the third (final) phase of normalize IR,
    after tracing

    - All CJUMP will be followed by the block containing the false
      branch.
    - There are no JUMP that is immediately followed by LABEL it jumps
      to

    Implementation: Basic blocks has formed a directed graph. Tracing
    is traversing by DFS.
*)

open Batteries

(** [trace_blocks blocks] reorders the basic block list given by
    [blocks] starting from the first block using DFS order. It returns
    the traversing path.  *)
let trace_blocks blocks : Basic_block.t list =
  let start = List.first blocks in
  let open Basic_block in
  (* map label -> basic_block for fast fetch *)
  let labelmap = create_labelmap blocks in
  let get_block lab =
    if Temp.LabelMap.mem lab labelmap then
      Some (Temp.LabelMap.find lab labelmap)
    else
      None
  in
  (* DFS visited marker, and a push function *)
  let next = Stack.create () in
  let push_label lab =
    match get_block lab with
    | None -> ()
    | Some (b) -> Stack.push b next
  in
  (* the main traverse function. *)
  let rec dfs visit_set path =
    let visited b = BBlockSet.mem b visit_set in
    if Stack.is_empty next then
      List.rev path (* get the result *)
    else
      let cur = Stack.pop next in
      if visited cur then
        dfs visit_set path
      else
        let visit_set' = BBlockSet.add cur visit_set in
        let path' = cur :: path in
        let jump_stmt = get_jump_stmt cur in
        match jump_stmt with
        | Ir.JUMP (_, labs) ->
          List.iter (fun l -> push_label l) labs;
          dfs visit_set' path'
        | Ir.CJUMP (_, _, _, t, f) ->
          push_label t;
          push_label f;
          dfs visit_set' path'
        | _ -> failwith "unreachable"
  in
  (* Start *)
  push_label start.label;
  dfs BBlockSet.empty []

(** Given a list of blocks, clean fall-through jumps and single jump block in the
    blocks.  Because this function breaks the invariant of basic block
    (i.e. there must be a jump in the end), this function shall be
    called as the last operation preparing IR for instruction
    selection.

    For fall through jump like label1: ... JUMP(label2) label2: ...

    In this situation, JUMP(label2) will be removed.

    For single jump block like label1: JUMP(labelx) label2: ...

    any jump jumping to label1 needs to jump to the final destination
    of labelx (which can be another jump).

    We do cleaning single jump first, and then fall-through jumps
*)
let clean_jumps bbs =
  let open Basic_block in
  let labelmap = Basic_block.create_labelmap bbs in
  let clean_fall_through_blocks bbs =
    let clean_fall_through blk1 rest =
      match rest with
      | [] -> [blk1]
      | blk2 :: tl ->
        let elms, last = Util.split_last blk1.stmts in
        begin match last with
          | Ir.JUMP(Ir.NAME(l), _) when l = blk2.label ->
            create (Ir.LABEL blk1.label :: elms) :: rest
          | _ -> blk1 :: rest
        end
    in
    let result = List.fold_right clean_fall_through bbs [] in
    result
  in
  let clean_single_jump_block bbs =
    (* invariant still holds for clean_single_jump_block's input
       block *)
    let rec jump_dest lab =
      match Temp.LabelMap.find_opt lab labelmap with
      | Some (bb) when List.length bb.stmts = 1 ->
        begin match List.hd bb.stmts with
          | Ir.JUMP (Ir.NAME(l), _) -> jump_dest l
          | _ -> lab
        end
      | _ -> lab
    in
    let chase_dest bb =
      let elms, last = Util.split_last bb.stmts in
      match last with
      | Ir.CJUMP (op, e1, e2, t, f) ->
        create (Ir.LABEL bb.label :: elms @
                [Ir.CJUMP(op, e1, e2, jump_dest t, jump_dest f)])
      | Ir.JUMP (Ir.NAME(l), _) ->
        let new_lab = jump_dest l in
        create (Ir.LABEL bb.label :: elms @
                [Ir.JUMP(Ir.NAME(new_lab), [new_lab])])
      | _ -> failwith "unreachable"
    in
    List.map chase_dest bbs
    |> trace_blocks (* clean unused jump block *)
  in
  clean_single_jump_block bbs
  |> clean_fall_through_blocks


(** For any block whose CJUMP's true label is the next block's label,
    flip the relation operation in CJUMP *)
let flip_cjump (bbs : Basic_block.t list) =
  let open Basic_block in
  (* Invarant: result's length >= 1 *)
  let result =
    List.fold_left
      (fun result b2 ->
         let b1 = List.hd result in
         let elm, last = Util.split_last b1.stmts in
         match last with
         | Ir.CJUMP (_, _, _, t, _) when t = b2.label ->
           let new_cjump = Ir.flip_cjump last in
           let new_b1 = Basic_block.create (Ir.LABEL(b1.label) :: elm
                                            @ [new_cjump]) in
           b2 :: new_b1 :: (List.tl result)
         | _ -> b2 :: result
      ) [List.hd bbs] (List.tl bbs)
  in
  List.rev result

(** The main entry for tracing blocks. In the end, the exit label is
    appended to make the program complete. *)
let trace_schedule (bbs, exit_label) =
  let bbs' = trace_blocks bbs
             |> clean_jumps
             |> flip_cjump in
  Basic_block.validate_jumps bbs';
  List.flatten (Basic_block.to_stmts bbs')  @ [Ir.LABEL(exit_label)]
