type t = {
  label : Temp.label; (** The label of this block *)
  stmts: Ir.stmt list; (** Instructions in this block *)
}
(** Basic block is the basic construct for optimization. It ensures

    - The first instruction is a LABEL
    - The last instruction is a JUMP or CJUMP
    - There is no other labels or jumps in a basic block
*)


val basic_blocks : Ir.stmt list -> t list * Temp.label
(** This function construct basic blocks from a linearized
    instructions (IRs), and returns an additional label that the last
    basic block will jump to.
*)


val basic_block_to_doc : t -> Pprint.doc

val basic_blocks_to_doc : t list -> Pprint.doc

val to_stmts : t list -> Ir.stmt list list
