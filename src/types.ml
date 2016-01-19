open Symbol

type t =
  | INT
  | STRING
  | RECORD of (Symbol.t * t) list
  | ARRAY of t
  | NIL
  | UNIT
  | NAME of Symbol.t * t option ref

type typ =
  | VarType of t (** Var Type *)
  | FuncType of t list * t (** argumentTypes * returnType *)

let t_to_string = function
  | INT -> "int"
  | STRING -> "string"
  | RECORD _ -> "record"
  | ARRAY _ -> "array"
  | NIL -> "nil"
  | UNIT -> "unit"
  | NAME (s, _)-> Symbol.to_string s

let typ_to_string = function
  | VarType (t) -> t_to_string t
  | FuncType (_) -> "function"

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

let valEnv : valEnv = SymbolTable.empty
