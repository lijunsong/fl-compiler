(** Activation Record. This interface specifies common traits of all
    targets. *)

type register = string

type frame

type access

type frag =
  | PROC of Ir.stmt * frame
  | STRING of Temp.label * string

(** debug *)
val frag_to_string: frag -> string

(** all register names for the target machines *)
val registers: register list

(** temporaries that already maps to machine registers *)
val temp_map: register Temp.TempMap.t

(** given a name, return the register *)
val temp_of_register : register -> Temp.temp

(** given a register, return its pre-defined name. If it has no
    predefined name, return None. *)
val register_of_temp : Temp.temp -> register option

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

(** [rv] is the register used to return value from callee
    function. Don't use it to fetch the return value from caller,
    for the view-shift on different platforms. *)
val rv : Temp.temp

val caller_save : register list

val callee_save : register list

(** the bias info on Sparc. TODO: this info should not be exported
    from here if we are going to do multiple backends *)
val bias : int

(** the size of a word in a Frame *)
val word_size : int

(** [get_access_exp base access] given an access and the base location
    of the access, this function returns the IR representing that
    location's content *)
val get_access_exp : Ir.exp -> access -> Ir.exp

(** [external_call f args] call external function f with args *)
val external_call : string -> Ir.exp list -> Ir.exp

(** implement view shift. Mainly called by Translate.proc_entry_exit *)
val view_shift : frame -> Ir.stmt -> Ir.stmt

(** after codegen, this function marks special registers for coloring.
 * This is called after codegen. *)
val proc_entry_exit2 : frame -> Assem.instr list -> Assem.instr list

(** genearte assembly prologue and epilogue. *)
val add_prolog_epilog : frame -> string list -> string list

(** debugging frame information *)
val frame_to_string : frame -> string

(** [string label str] generates data section for [str] which will
    be [label]ed *)
val string : Temp.label -> string -> string


