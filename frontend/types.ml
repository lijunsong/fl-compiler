open Symbol
open Translate
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
type t =
  | INT
  | STRING
  | RECORD of (Symbol.t * t) list * Uniq.t
  | ARRAY of t * Uniq.t
  | NIL      (* NIL is normally used as the type of nil, as var a : record = nil *)
  | UNIT     (* UNIT is the type of an empty sequence. *)
  | NAME of Symbol.t * t option ref

type typ =
  | VarType of Translate.access * t
  (** Var access * Var Type *)

  | FuncType of Translate.level * t list * t
  (** function's  FunctionOwnLevel * argumentTypes * returnType
      NOTE: each level has already associated a label *)

let t_to_string = function
  | INT -> "int"
  | STRING -> "string"
  | RECORD _ -> "record"
  | ARRAY _ -> "array"
  | NIL -> "nil"
  | UNIT -> "unit"
  | NAME (s, _)-> Symbol.to_string s

let typ_to_string = function
  | VarType (_, t) -> t_to_string t
  | FuncType (l, _, _) -> "function" ^ (Translate.get_label l |> Temp.label_to_string )

let rec record_find (lst : (Symbol.t * t) list) (sym : Symbol.t) : t option =
  match lst with
  | [] -> None
  | (sym', t) :: tl ->
     if sym' = sym then Some(t)
     else record_find tl sym

(** typeEnv stores type-id -> type *)
type typeEnv = t SymbolTable.t

(** valEnv stores identifier -> type *)
type valEnv = typ SymbolTable.t

(** typeEnv includes predefined types: int string *)
let typeEnv : typeEnv =
  let predefined = [("int", INT); ("string", STRING)] in
  List.fold_right (fun (s, t) table ->
      SymbolTable.enter (Symbol.of_string s) t table)
                  predefined SymbolTable.empty

(** valEnv predefines built-in functions. *)
let valEnv : valEnv =
  ["print", FuncType(Translate.outermost, [STRING], UNIT);
   "getchar", FuncType(Translate.outermost, [], STRING);
   "ord", FuncType(Translate.outermost, [STRING], INT);
   "chr", FuncType(Translate.outermost, [INT], STRING);
   "size", FuncType(Translate.outermost, [STRING], INT);
   "substring", FuncType(Translate.outermost, [STRING; INT; INT], STRING);
   "concat", FuncType(Translate.outermost, [STRING; STRING], STRING);
   "not", FuncType(Translate.outermost, [INT], INT);
   "exit", FuncType(Translate.outermost, [INT], UNIT);
  ]
  |> List.map (fun (n,t) -> Symbol.of_string n, t)
  |> List.enum
  |> SymbolTable.of_enum

(** TODO: make SymbolTable a functor, typeEnv/valEnv submodule and
 * debug_print automatically works. *)
let debug_typeEnv (env : typeEnv) =
  Printf.printf "--- typeEnv ---\n";
  SymbolTable.debug_print t_to_string env

let debug_valEnv (env : valEnv) =
  Printf.printf "--- valEnv ---\n";
  SymbolTable.debug_print typ_to_string env
