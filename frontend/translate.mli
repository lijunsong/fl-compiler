open Symbol
open Sparc

(** This module translates Syntax to Ir.
 *
 * For the toplevel and each function, the translation arranges local
 * variables and produces Ir for each expression. It also remembers
 * all string literals and declared functions.
 *)

(** TODO: make translate a functor taking Frame as input module *)

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
val new_level : ?add_static_link:bool -> level -> Temp.label -> bool list -> level

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

val break: Temp.label -> exp

val no_value: unit -> exp

(** [binop op operand1 operand2] *)
val binop : Ir.binop -> exp -> exp -> exp

(** [relop op operand1 operand2] *)
val relop : Ir.relop -> exp -> exp -> exp

(** [assign lhs rhs] *)
val assign : exp -> exp -> exp

(** [call def_level use_level args] *)
val call : level -> level -> exp list -> exp

val record : exp list -> exp

val seq : (exp * 'a) list -> (exp * 'a)

val if_cond_unit_body : exp -> exp -> exp option -> exp

val if_cond_nonunit_body : exp -> exp -> exp option -> exp

val while_loop : exp -> exp -> Temp.label -> exp

val array : exp -> exp -> exp

val let_body : exp list -> exp -> exp

(** [proc_entry_exit level fbody] remembers the [fbody] of [level] as
    a fragment.

    Call [get_result] to get all fragments.  This is the main function
    to implement prologue and epilogue of functions
*)
val proc_entry_exit : ?is_procedure:bool -> level -> exp -> unit

val get_result : unit -> frag list
