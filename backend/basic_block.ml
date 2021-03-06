open Batteries

type t = {
  label : Temp.label; (** The label of this block *)
  stmts: Ir.stmt list; (** Instructions in this block *)
}
(** Basic block is the basic construct for optimization. It ensures

    - The first instruction is a LABEL
    - The last instruction is a JUMP or CJUMP
    - There is no other labels or jumps in a basic block
*)

type basic_block_t = t

module BBlock = struct
  type t = basic_block_t
  let compare a b = compare a.label b.label
end

module BBlockMap = Map.Make(BBlock)

module BBlockSet = Set.Make(BBlock)

let create_labelmap bbs =
  List.map (fun bb -> bb.label, bb) bbs
  |> Temp.LabelMap.of_list

let get_jump_stmt bb =
  match List.last bb.stmts with
  | Ir.CJUMP(_)
  | Ir.JUMP (_) as e -> e
  | _ -> failwith ("last stmt isn't JUMP/CJUMP in block " ^
                   Temp.label_to_string bb.label)

let basic_block_to_doc bb =
  let open Pprint in
  let to_doc bb =
    let lab = Temp.label_to_string bb.label in
    text lab
    <-> nest (String.length lab)
      (line <-> (concat line (List.map Ir.stmt_to_doc bb.stmts)))
  in
  to_doc bb

let basic_blocks_to_doc bbs =
  let open Pprint in
  concat line (List.map basic_block_to_doc bbs)

(** Create a basic block from a sequence of IR statements. The first
    ir must be a label and the last ir must be a JUMP or CJUMP. *)
let create stmts =
  match stmts with
  | Ir.LABEL(l) :: rest ->
    {label = l; stmts = rest}
  | _ -> failwith "No label found to construct a basic block."

let is_jump = function
  | Ir.CJUMP (_)
  | Ir.JUMP (_) -> true
  | _ -> false

(** verify the invariant of a basic block (only valid for basic block
    construction): No label (because label is in part of the struct)
    and has only one jump-like IR. See also validate_jumps *)
let verify_basic_blocks bbs =
  let is_label = function
    | Ir.LABEL(_) -> true
    | _ -> false
  in
  let verify bb =
    let labels = List.filter is_label bb.stmts in
    let jumps = List.filter is_jump bb.stmts in
    assert (List.length labels = 0);
    assert (List.length jumps = 1)
  in
  List.iter verify bbs

let validate_jumps bbs =
  let labelmap = create_labelmap bbs in
  let validate lab = not (Temp.LabelMap.mem lab labelmap) in
  let invalid = List.filter (fun bb ->
      try
        begin match get_jump_stmt bb with
          | Ir.CJUMP(_, _, _, t, f) ->
            validate t && validate f
          | Ir.JUMP (_, ls) ->
            List.for_all validate ls
          | _ -> false
        end
      with _ -> false) bbs
  in
  match invalid with
  | [] -> ()
  | [hd] -> () (* the exit label is not mapped *)
  | _ ->
    Pprint.print_doc (basic_blocks_to_doc invalid);
    raise (Failure(Printf.sprintf "There are %d invalid jumps."
                  (List.length invalid)))





(** invariant: the first stmt in [stmts] must be a LABEL. *)
let munch_basic_block exit_label stmts : t * Ir.stmt list =
  let rec munch stmts cur_block =
    match stmts with
    | [] ->
      (* reach the end of stmts, jump to exit_label. The last stmt in
         cur_block can't be a jump-like stmt because the front end
         will always generate a label after a jump. *)
      let jmp_exit = Ir.JUMP(Ir.NAME(exit_label), [exit_label]) in
      let rev = List.rev (jmp_exit :: cur_block) in
      create rev, []

    | Ir.LABEL(l) :: rest when List.is_empty cur_block ->
      (* the openning stmt is a label *)
      munch rest [Ir.LABEL(l)]

    | fst :: rest when List.is_empty cur_block ->
      (* the openning stmt is not a label, add a label *)
      munch stmts [Ir.LABEL(Temp.new_label())]

    | Ir.LABEL(l) :: rest ->
      (* No jump found for the end of block, construct a jump *)
      let jump = Ir.JUMP(Ir.NAME(l), [l]) in
      let rev = List.rev (jump :: cur_block) in
      create rev, stmts

    | e :: rest when is_jump e ->
      let rev = List.rev (e :: cur_block) in
      create rev, rest

    | e :: rest ->
      munch rest (e :: cur_block)
  in
  munch stmts []

let basic_blocks linearized =
  let exit_label = Temp.new_label ~prefix:"exit" () in
  let rec make result stmts =
    if stmts = [] then
      List.rev result
    else
      let block, rest = munch_basic_block exit_label stmts in
      make (block :: result) rest
  in
  let blocks = make [] linearized in
  verify_basic_blocks blocks;
  blocks, exit_label

let to_stmts bbs =
  let bb_to_stmts bb =
    Ir.LABEL(bb.label) :: bb.stmts
  in
  List.map bb_to_stmts bbs

let join blk1 blk2 : t list =
  let stmt, jump = Util.split_last blk1.stmts in
  match jump with
  | Ir.CJUMP (op, e0, e1, t, f) when blk2.label = f ->
    begin match blk2.stmts with
      | [Ir.JUMP(Ir.NAME(l), _)] ->
        let new_stmts = Ir.LABEL(blk1.label) :: stmt @ [Ir.CJUMP(op, e0, e1, t, l)] in
        [create new_stmts]
      | _ ->
        [blk1; blk2]
    end
  | _ ->
    [blk1; blk2]
