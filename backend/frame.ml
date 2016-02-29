(** Machine-independent activation record. *)
module type Frame = sig
  type register = string with sexp

  type frame with sexp

  type access with sexp

  type frag =
    | PROC of Ir.stmt * frame
    | STRING of Temp.label * string
  with sexp

  (** all register names for the target machines *)
  val registers: register list

  (** given a name, return the register *)
  val get_register : register -> Temp.temp

  (** [new_frame name formals] create a frame named l. A list of
      bool indicates whether each formal argument escapes. *)
  val new_frame : Temp.label -> bool list -> frame

  (** retrieve the given frame's name *)
  val get_name : frame -> Temp.label

  (** retrieve the given frame's formal arguments.  *)
  val get_formals : frame -> access list

  (** [alloc_local f escape] allocate a local variable on frame [f] with
  [escape] indicating whether the variable escapes *)
  val alloc_local : frame -> bool -> access

  val fp : Temp.temp

  val rv : Temp.temp

  (** the size of a word in a Frame *)
  val word_size : int

  (** [get_exp base access] given the base location of the access,
  this function returns the IR representing that location's content *)
  val get_exp : Ir.exp -> access -> Ir.exp

  (** [external_call f args] call external function f with args *)
  val external_call : string -> Ir.exp list -> Ir.exp

  (** implement view shift. Mainly called by Translate.proc_entry_exit *)
  val proc_entry_exit1 : frame -> Ir.stmt -> Ir.stmt

  (** after codegen, this function marks special registers for coloring.
   * This is called after codegen. *)
  val proc_entry_exit2 : frame -> Assem.instr list -> Assem.instr list

  (** genearte assembly prologue and epilogue, this is called after
   *  codegen. *)
  val proc_entry_exit3 : frame -> string list -> string list

  (** dump frame information for debugging *)
  val debug_dump : frame -> unit
end

