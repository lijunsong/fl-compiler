open Symbol

(** This module translates Syntax to Ir.
 *
 * For the toplevel and each function, the translation arranges local
 * variables and produces Ir for each expression. It also remembers
 * all string literals and declared functions.
 *)

(** TODO: make translate a functor taking Frame as input module *)
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

  (** dump frame information for debugging *)
  val debug_dump : frame -> unit
end

module SparcFrame : Frame

module F = SparcFrame

(** Each nested function declared in Tiger's [let] is in a deeper
    level *)
type level

type access

type exp =
  | Ex of Ir.exp
  | Nx of Ir.stmt
  | Cx of (Temp.label -> Temp.label -> Ir.stmt) with sexp

type frag = F.frag with sexp

val unEx : exp -> Ir.exp

val unNx : exp -> Ir.stmt

val unCx : exp -> Temp.label -> Temp.label -> Ir.stmt

val outermost : level

(** [new_level parent name formals ] create a new level for a nested
    function. *)
val new_level : level -> Temp.label -> bool list -> level

val get_formals : level -> access list

val get_label : level -> Temp.label

(** [alloc_local level escape] allocate a local variable on [level]
  with [escape] indicating whether the variable escapes. *)
val alloc_local : level -> bool -> access

val debug_print : unit -> unit

(** The following functions provides interface to create [exp] from
    source language *)

val const : int -> exp

val string : string -> exp

(** [simple_var acc use_level] given the access of a variable and
where it is using, return an Ir of var's variable location *)
val simple_var : access -> level -> exp


(** [var_field base fld fld_list] given the expression of base
location of the struct, the fld symbol and a symbol list, return an exp
option that calculates the fld location, and the associated value of
that field. None if the fld is not found *)
val var_field : exp -> Symbol.t -> (Symbol.t * 'a) list -> (exp * 'a) option

(** [var_subscript base index] returns the location of that index. *)
val var_subscript : exp -> exp -> exp

val nil : unit -> exp

val break: unit -> exp

val no_value: unit -> exp

(** [binop op operand1 operand2] *)
val binop : Ir.binop -> exp -> exp -> exp

(** [relop op operand1 operand2] *)
val relop : Ir.relop -> exp -> exp -> exp

(** [assign lhs rhs] *)
val assign : exp -> exp -> exp

(** [call f args] *)
val call : level -> exp list -> exp

val record : exp list -> exp

val seq : (exp * 'a) list -> (exp * 'a)

val if_cond_unit_body : exp -> exp -> exp option -> exp

val if_cond_nonunit_body : exp -> exp -> exp option -> exp

val while_loop : exp -> exp -> exp

val array : exp -> exp -> exp

(** [prepend_stmts lst e] Prepend a list of statements [lst] to the
given [e] *)
val prepend_stmts : exp list -> exp -> exp

(** [proc_entry_exit level fbody] remembers the [fbody] of [level] as
    a fragment.

    Call [get_result] to get all fragments.  This is the main function
    to implement prologue and epilogue of functions
*)
val proc_entry_exit : level -> exp -> unit

val get_result : unit -> frag list
