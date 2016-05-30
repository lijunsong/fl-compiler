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

(** [follow_false_label blocks] traverse the basic block list given by
    [blocks] starting from the first block. It returns the traversing
    path *)
let follow_false_label blocks : Basic_block.t list =
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

(** Given a list of blocks, apply [Basic_block.join] on all blocks. *)
let clean_fall_through_jump bbs = match bbs with
  | [] -> bbs
  | hd :: [] -> bbs
  | _ ->
    let len = List.length bbs in
    let elms, last = List.split_at (len-1) bbs in
    List.fold_right (fun blk1 rest ->
        (* the first arg of @ is of length 1 or 2, so the performance
           is good. *)
        (Basic_block.join blk1 (List.hd rest)) @ (List.tl rest))
      elms last

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

let trace_schedule (bbs, exit_label) =
  let bbs' = follow_false_label bbs in
  Basic_block.compute_control_flow bbs';
  let bbs'' = clean_fall_through_jump bbs'
              |> flip_cjump in
  Basic_block.compute_control_flow bbs'';
  Pprint.print_doc (Basic_block.basic_blocks_to_doc bbs'');
  List.flatten (Basic_block.to_stmts bbs'')
