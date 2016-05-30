(** Description: This is the second phase of normalize IR. This phase
    will create basic block (bb) from a set of statements.  Having
    called basic_blocks, program will be splited into basic
    blocks. For each basic block

    - The first instruction is a LABEL
    - The last instruction is a JUMP or CJUMP
    - There is no other labels or jumps in a basic block
*)
open Batteries

type t = {
  label : Temp.label; (** The label of this block *)
  stmts: Ir.stmt list; (** Instructions in this block *)
  mutable pred : t list; (** predecessors *)
  mutable succ : t list; (** successors *)
}


module BBlockMap : BatMap.S with type key = t

module BBlockSet : BatSet.S with type elt = t

val create : Ir.stmt list -> t
(** Create a basic block from a sequence of IR statements. The first
    ir must be a label and the last ir must be a JUMP or CJUMP. *)

val create_labelmap : t list -> t Temp.LabelMap.t
(** Given a list of basic blocks, create a hash map for label to basic
    block mapping. *)

val get_jump_stmt : t -> Ir.stmt
(** [get_jump_stmt bb] get the jump (CJUMP or JUMP) statement in the
    basic block [bb].

    @raise Failure if no such statement exists (Basic Block invariant
    is broken)*)

val basic_blocks : Ir.stmt list -> t list * Temp.label
(** This function construct basic blocks from a linearized
    instructions (IRs), and returns an additional label that the last
    basic block will jump to.
*)

val compute_control_flow : t list -> unit
(** generate predecessor and successor info for each block *)

val basic_block_to_doc : t -> Pprint.doc

val basic_blocks_to_doc : t list -> Pprint.doc

val to_stmts : t list -> Ir.stmt list list

val join : t -> t -> t list
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
