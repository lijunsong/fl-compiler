module S = Syntax
open Sexplib
open Parse
open Printf

let _ =
  let ast = Parse.parse_stdin () in
  let sexp = S.sexp_of_exp ast in
  let _ = Sexp.output_hum stdout sexp in
  try
    Semant.transProg ast
  with
  | Semant.TypeError (pos, msg) ->
     printf "TypeError:%s: %s" (Pos.to_string pos) msg
  | Semant.UndefinedError (pos, msg) ->
     printf "TypeError:%s: %s" (Pos.to_string pos) msg
