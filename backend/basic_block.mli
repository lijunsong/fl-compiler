(** Description: This is the second phase of normalize IR,
    after basic_blocks, program will be splited into basic blocks. For each basic block

    - The first instruction is a LABEL
    - The last instruction is a JUMP or CJUMP
    - There is no other labels or jumps in a basic block
*)
open Batteries

type t = {
  label : Temp.label; (** The label of this block *)
  stmts: Ir.stmt list; (** Instructions in this block *)
}

module BBlockMap : BatMap.S with type key = t

module BBlockSet : BatSet.S with type elt = t

val create : Ir.stmt list -> t

val basic_blocks : Ir.stmt list -> t list * Temp.label
(** This function construct basic blocks from a linearized
    instructions (IRs), and returns an additional label that the last
    basic block will jump to.
*)

val basic_block_to_doc : t -> Pprint.doc

val basic_blocks_to_doc : t list -> Pprint.doc

val to_stmts : t list -> Ir.stmt list list

(** [join blk1 blk2] joins two blocks for two situation:

    1. If the jump in [blk1] is JUMP to the label of [blk2], this
    function joins the two blocks by eliminating the JUMP and LABEL in
    the middle.

    2. If the jump in [blk1] is CJUMP and [blk2] is the false branch
    with a single [JUMP(NAME(l))] as its body, this function
    eliminates [blk2] and replacing the [blk1]'s false label with [l]

    It returns the two blocks if joining is not applicable.

    To join a list of blocks, use List.fold_right for one pass.
*)
val join : t -> t -> t list
