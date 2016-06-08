(** Description: This is the third (final) phase of normalize IR,
    after tracing

    - All CJUMP will be followed by the block containing the false
      branch.
    - There are no JUMP that is immediately followed by LABEL it jumps
      to

    Implementation: Basic blocks has formed a directed graph. Tracing
    is traversing the graph in DFS order.
*)

(** Given basic blocks and a exit label, normalize the IR in blocks. *)
val trace_schedule : Basic_block.t list * Temp.label -> Ir.stmt list
