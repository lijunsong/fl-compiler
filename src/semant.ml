module S = Syntax

module Translate = struct
  type exp = unit
end

(** Type Chcker

Notation:
 - "te": type environment
 - "te |- E : T": expression E in type envionrment te has type T
 - "te(I)" : designates the type assigned to symbol I in te
               premise1, premise2, etc.
 - type rule  -------------------------
                   conclusion


 1. te |- Int : INT
 2. te |- String : STRING
 3. te |- Nil : NIL
 4. te |- Var(VarId(s)) : te(s)

 5. te |- var : RECORD([fld: T]),
    te |- fld : STRING
    ------------------------------------------------
    te |- Var(VarField(var, fld)) : T

 6. te |- var : ARRAY of T,
    te |- E : INT
    ----------------------
    te |- Var(VarSubscript(var, E))

 **)

type expty = Types.t

let transExp (tenv : typeEnv) (venv : valEnv) (exp : S.exp) : expty =
  match exp with
  | S.String (_) -> Types.STRING
  | S.Int (_) -> Types.INT
  | S.Nil (_) -> Types.NIL
