(**
   This file sets up initial type environment and value environment.

   Environment is used only in type checking.
*)

open Types

module SymbolTable = Symbol.SymbolTable

(** type ids *)
let prelude_type =
  [
    ("int", INT);
    ("string", STRING);
  ]

(** built-in function ids *)
let prelude_val =
  [
    "print", [STRING], UNIT;
    "getchar", [], STRING;
    "ord", [STRING], INT;
    "chr", [INT], STRING;
    "size", [STRING], INT;
    "substring", [STRING; INT; INT], STRING;
    "concat", [STRING; STRING], STRING;
    "not", [INT], INT;
    "exit", [INT], UNIT;
    "assert", [INT], UNIT;
  ]

type value_env = val_ty SymbolTable.t
(** value_env maps variable id and function id to its type *)

type type_env = ty SymbolTable.t
(** type_env maps type id to types *)

let init_value_env = SymbolTable.empty

let init_type_env = SymbolTable.empty
