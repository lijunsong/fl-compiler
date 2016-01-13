open Lexer
open Parser
module S = Syntax
open Sexplib

let _ =
    let lexbuf = Lexing.from_channel stdin in
    let result = prog tokenize lexbuf in
    let sexp = S.sexp_of_exp result in
    Sexp.output_hum stdout sexp
