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

    TODO: consider to return option. After all invariant only holds
    before cleaning jumps in trace.

    @raise Failure if no such statement exists (Basic Block invariant
    is broken)*)

val basic_blocks : Ir.stmt list -> t list * Temp.label
(** This function construct basic blocks from a linearized
    instructions (IRs), and returns an additional label that the last
    basic block will jump to.
*)

val basic_block_to_doc : t -> Pprint.doc

val basic_blocks_to_doc : t list -> Pprint.doc

val to_stmts : t list -> Ir.stmt list list

val validate_jumps : t list -> unit
(** assert all jump statement (except the jump to exit) has a valid
    label to jump to *)
