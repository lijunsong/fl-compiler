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

type typeEnv = t SymbolTable.t

type valEnv = typ SymbolTable.t

let typeEnv : typeEnv = SymbolTable.empty

let valEnv : valEnv = SymbolTable.empty
