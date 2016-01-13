open Sexplib.Std
open Sexplib.Conv

module Pos =
  struct
    type t = {start_p : Lexing.position; end_p : Lexing.position}

    let dummy = {start_p = Lexing.dummy_pos; end_p = Lexing.dummy_pos}

    let from_lexbuf lexbuf : t =
      { start_p = lexbuf.Lexing.lex_start_p;
        end_p = lexbuf.Lexing.lex_curr_p
      }

    let get_column (p : Lexing.position) : int =
      p.Lexing.pos_cnum - p.Lexing.pos_bol

    let to_string (p : t) : string =
      Printf.sprintf "%d:%d-%d:%d" p.start_p.Lexing.pos_lnum (get_column p.start_p)
                     p.end_p.Lexing.pos_lnum
                     (get_column p.end_p)

  end
