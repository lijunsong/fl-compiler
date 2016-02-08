open Parser
open Lexer
module S = Syntax

module Parse = struct
    (** [row col token] indicates where the error happens. *)
    exception ParseError of int * int * string

    (** [parse_string lexbuf] parses the given lexbuf and returns Tiger abstarct syntax.
        @raise ParseError when parser fails. *)
    let parse_lexbuf lexbuf : S.exp =
      try
        prog tokenize lexbuf
      with
      | Parsing.Parse_error ->
         let pos = lexbuf.Lexing.lex_start_p in
         let row = pos.Lexing.pos_lnum in
         let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
         raise (ParseError(row, col, Lexing.lexeme lexbuf))
      | Failure msg -> failwith msg

    (** [parse_string s] same as parse_string *)
    let parse_string (s : string) : S.exp =
      let lexbuf = Lexing.from_string s in
      parse_lexbuf lexbuf

    (** [parse_stdin c] same as parse_stdin *)
    let parse_stdin () : S.exp =
      let lexbuf = Lexing.from_channel stdin in
      parse_lexbuf lexbuf
end
