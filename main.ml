open Lexer
open Parser

let _ =
    let lexbuf = Lexing.from_channel stdin in
    let _ = prog tokenize lexbuf in
    1
