(** TODO: make translate a functor taking Frame as input module *)

(** Each nested function declared in Tiger's [let] is in a deeper
    level *)
type level

type access

type exp =
  | Ex of Ir.exp
  | Nx of Ir.stmt
  | Cx of (Temp.label -> Temp.label -> Ir.stmt)

val dummy_exp : exp

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

(** The following functions provides interface to create [exp] from
    source language *)

val const : int -> exp
