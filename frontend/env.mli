open Types

module SymbolTable = Symbol.SymbolTable

type value_env = val_ty SymbolTable.t
(** value_env maps variable id and function id to its type *)

type type_env = ty SymbolTable.t
(** type_env maps type id to types *)

val init_value_env : value_env
(** initial value environment *)

val init_type_env : type_env
(** initial type environment *)
