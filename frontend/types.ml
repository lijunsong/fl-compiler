(**
   This file implements type environment and value environment of type checking.

   Tiger has two namespaces. One for type identifiers, like int, string.
   Another one is for value, like variables and functions.

   Separating the two environments allows an identifier to be used as a
   type id as well as a variable or a function name at the same time.

   Type information is only used during type checking. Translating AST
   to IR doesn't need type information.
*)

open Batteries

(** uniq is to differentiate records (and arrays) that have similar
 * fields *)
module Uniq : sig
  type t
  val uniq : unit -> t
  val compare : t -> t -> int
end = struct
  type t = int
  let next = ref 0
  let compare = compare
  let uniq () =
    incr next;
    !next
end

(* TODO: explain the necessary to have NIL and UNIT both as a type. *)
type ty =
  | INT
  | STRING
  | RECORD of (Symbol.t * ty) list * Uniq.t
  | ARRAY of ty * Uniq.t
  | NIL      (* NIL is normally used as the type of nil, as var a : record = nil *)
  | UNIT     (* UNIT is the type of an empty sequence. *)
  | NAME of Symbol.t * ty option ref

let rec record_find (lst : (Symbol.t * ty) list) (sym : Symbol.t) : ty option =
  match lst with
  | [] -> None
  | (sym', t) :: tl ->
     if sym' = sym then Some(t)
     else record_find tl sym

(** [t] models the type of variables and functions. *)
type val_ty =
  | VarType of Translate.access * ty
  (** Var access * Var Type *)

  | FuncType of Translate.level * ty list * ty
  (** function's  FunctionOwnLevel * argumentTypes * returnType
      NOTE: each level has already associated a label *)


let ty_to_string = function
  | INT -> "int"
  | STRING -> "string"
  | RECORD _ -> "record"
  | ARRAY _ -> "array"
  | NIL -> "nil"
  | UNIT -> "unit"
  | NAME (s, _)-> Symbol.to_string s

let val_ty_to_string = function
  | VarType (_, t) -> ty_to_string t
  | FuncType (_, argty, ret) -> "Function"
