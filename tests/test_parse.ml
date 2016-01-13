open OUnit2
open Lexer
open Parser
open Pos
open Utils
open Sexplib
module S = Syntax

(** This function ignores Pos field in AST comparison *)
let assert_parse (s : string) (expected : S.exp) =
  let test ctx =
    let lexbuf = Lexing.from_string s in
    let ast' = prog tokenize lexbuf in
    assert_equal ~cmp:ast_equal expected ast'
                 ~printer:(fun a -> Sexp.to_string (S.sexp_of_exp a))
  in
  test

let dummy = Pos.dummy

(** Because the parsing must always produce an exp, testing declations
and l-values are done through testing Let exp *)
let suite =
  "Lexing and Parsing Tests" >::: [
      "Number" >::
        (assert_parse "12" (S.Int(dummy, 12)))
    ;

      "built-in type declaration" >::
        assert_parse
          "let type myint = int in 1 end"
          (S.Let(dummy,
                 [S.TypeDecl(
                      [dummy, "myint", S.NameTy(dummy, "int")])],
                 S.Int(dummy, 1)))
    ;
      "record type declaration" >::
        assert_parse
          "let type myint = { lo: int, mid: int, hi: int} in 1 end"
            (S.Let(dummy,
                   [S.TypeDecl([dummy, "myint",
                               S.RecordTy([{S.fldName= "lo"; S.ty = "int"; S.pos = dummy};
                                           {S.fldName= "mid"; S.ty = "int"; S.pos = dummy};
                                           {S.fldName= "hi"; S.ty = "int"; S.pos = dummy}])])],
                   S.Int(dummy, 1)))
    ;

      "record type decl [empty fields]" >::
        assert_parse
          "let type myint = {} in 1 end"
          (S.Let(dummy,
                [S.TypeDecl(
                     [dummy, "myint", S.RecordTy([])])],
                S.Int(dummy, 1)))
    ;

      "array type declaration" >::
        assert_parse
          "let type ary = array of int in 1"
          (S.Let(dummy,
                 [S.TypeDecl([dummy, "ary", S.ArrayTy(dummy, "int")])],
                 S.Int(dummy, 1)))
    ;

      "variable decl" >::
        assert_parse
          "let var x := 12 in x end"
          (S.Let(dummy,
                 [S.VarDecl(dummy, "x", None, S.Int(dummy, 12))],
                 S.Var(dummy, S.VarId("x"))))
    ;

      "varable decl" >::
        assert_parse
          "let var x : int := 12 in x end"
          (S.Let(dummy,
                 [S.VarDecl(dummy, "x", Some "int", S.Int(dummy, 12))],
                 S.Var(dummy, S.VarId("x"))))
    ;

    ]

let _ =
  run_test_tt_main suite
