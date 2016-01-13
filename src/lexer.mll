{
open Batteries
open Parser
open Lexing
open Pos
exception LexError of string * Lexing.position

let keywords : (string, token) Hashtbl.t = List.enum [
  ("array", ARRAY);
  ("break", BREAK);
  ("do", DO);
  ("else", ELSE);
  ("end", END);
  ("for", FOR);
  ("function", FUNCTION);
  ("if", IF);
  ("in", IN);
  ("let", LET);
  ("nil", NIL);
  ("of", OF);
  ("then", THEN);
  ("to", TO);
  ("type", TYPE);
  ("var", VAR);
  ("while", WHILE);
] |> Hashtbl.of_enum

let punctuation : (string, token) Hashtbl.t = List.enum [
  ("(", LP);
  (")", RP);
  ("{", LBRACE);
  ("}", RBRACE);
  ("[", LBRACKET);
  ("]", RBRACKET);
  (".", DOT);
  (",", COMMA);
  (":=", ASSIGN);
  (":", COLON);
  (";", SEMICOLON);
  ("+", PLUS);
  ("-", MINUS);
  ("*", TIMES);
  ("/", DIV);
  ("=", EQ);
  ("<=", LE);
  (">=", GE);
  ("<", LT);
  ("<>", NEQ);
  (">", GT);
  ("&", AND);
  ("|", OR);
] |> Hashtbl.of_enum

let is_keywords = Hashtbl.mem keywords
let is_punctuation = Hashtbl.mem punctuation

let token_of_keyword = Hashtbl.find keywords
let token_of_punctuation = Hashtbl.find punctuation

}


let letters = ['a'-'z''A'-'Z']
let ident = letters(letters | ['0'-'9''_'])*
let digit = ['0'-'9']
let integer_lit = digit+
let blank = ['\t' ' ']+
let newline = '\n'
let punc = '(' | ')' | '{' | '}' | '[' | ']' | '.' | ',' |
           ":=" | ':' | ';' | '+' | '-' | '*' | '/' | '=' |
           "<=" | ">=" | '<' | "<>" | '>' | '&' | '|'

(* the main entry for tokenize a program *)
rule tokenize = parse
 | ident  (* identifier and keywords *)
   { let s = lexeme lexbuf in
     if is_keywords s then token_of_keyword s
     else Id(s)
   }
 | integer_lit { Int (int_of_string (lexeme lexbuf)) }
 | "/*" (* comments *)
   { consume_comments lexbuf.lex_start_p lexbuf;
     tokenize lexbuf
   }
 | '"'
   { let buf = BatBuffer.create 32 in
     tokenize_string lexbuf.lex_start_p buf lexbuf
   }
 | blank { tokenize lexbuf }
 | newline { new_line lexbuf; tokenize lexbuf }
 | punc {
    let s = lexeme lexbuf in
    if is_punctuation s then token_of_punctuation s
    else failwith ("unexpected symbol: " ^ s)
   }
 | eof
   { EOF }
 | _ {
   let s = lexeme lexbuf in
   if is_keywords s then token_of_keyword s
   else failwith ("unexpected token " ^ s)
 }

and consume_comments start_pos = parse
 | "*/"  (* match comments end *)
   { }
 | "/*"  (* nested comments *)
   { consume_comments start_pos lexbuf }
 | eof  (* comments are not closed *)
   { failwith "comments not closed" }
 | newline  { new_line lexbuf; consume_comments start_pos lexbuf }
 | _ (* anything in comments is just ignored *)
   { consume_comments start_pos lexbuf }

and tokenize_string start_pos buf = parse
 | '"'      { lexbuf.lex_start_p <- start_pos;
              String (BatBuffer.contents buf) }
 | eof      { failwith "comments not closed" }
 | '\\' 'n' { BatBuffer.add_char buf '\n'; tokenize_string start_pos buf lexbuf }
 | '\\' 't' { BatBuffer.add_char buf '\t'; tokenize_string start_pos buf lexbuf }
 | '\\' digit digit digit
   { let c = lexeme lexbuf |> BatString.lchop |> BatString.to_int |> Char.chr in
     BatBuffer.add_char buf c;
     tokenize_string start_pos buf lexbuf
   }
 | '\\' '"'
   { BatBuffer.add_char buf '"';
     tokenize_string start_pos buf lexbuf
   }
 | '\\' '\\'
   { BatBuffer.add_char buf '\\';
     tokenize_string start_pos buf lexbuf
   }
 | newline { new_line lexbuf; BatBuffer.add_char buf '\n';
     tokenize_string start_pos buf lexbuf
   }
 | _ { BatBuffer.add_string buf (lexeme lexbuf);
       tokenize_string start_pos buf lexbuf }
