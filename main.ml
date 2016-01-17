module S = Syntax
open Sexplib
open Parse

let _ =
  let ast = Parse.parse_stdin () in
  let sexp = S.sexp_of_exp ast in
  Sexp.output_hum stdout sexp
