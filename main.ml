open Lexer
open Parser
module S = Syntax
open Sexplib

(** todo: use module type to wrap the parse function with exception handling *)
let print_error lexbuf =
    let pos = lexbuf.Lexing.lex_start_p in
    Printf.printf "syntax error %d:%d: %s\n" pos.Lexing.pos_lnum (pos.Lexing.pos_cnum-pos.Lexing.pos_bol) (Lexing.lexeme lexbuf)

let _ =
  let lexbuf = Lexing.from_channel stdin in
  try
    let result = prog tokenize lexbuf in
    let sexp = S.sexp_of_exp result in
    Sexp.output_hum stdout sexp
  with
  | Parsing.Parse_error -> print_error lexbuf
  | Failure msg -> failwith msg
