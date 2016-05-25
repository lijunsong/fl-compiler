(** This file implements type environment and value environment for
    type checking.

    Separating the two environments allows an identifier to be used as a
    type id as well as a variable or a function name at the same time.

    This file sets up initial type environment and value environment.

    Environment is used only in type checking.
*)

open Types
open Batteries

module SymbolTable = Symbol.SymbolTable

(** type ids *)
let prelude_type =
  [
    ("int", INT);
    ("string", STRING);
  ]

(** built-in function ids. each tuple is (function name, arg type,
    return type) *)
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

(** initial value environment *)
let init_value_env =
  List.map (fun (name, args_ty, ret_ty) ->
      (* pass all args on stack*)
      let formals = List.make (List.length args_ty) true in
      let level = Translate.new_level ~add_static_link:false
          Translate.outermost (Temp.named_label name) formals in
      Symbol.of_string name, FuncType(level, args_ty, ret_ty)
    ) prelude_val
  |> SymbolTable.of_list

(** initial type environment *)
let init_type_env =
  List.map (fun (name, ty) -> Symbol.of_string name, ty) prelude_type
  |> SymbolTable.of_list
