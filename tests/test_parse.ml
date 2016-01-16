open OUnit2
open Lexer
open Parser
open Pos
open Utils
open Sexplib
module S = Syntax
open Batteries

(** This function ignores Pos field in AST comparison *)
let assert_parse (s : string) (expected : S.exp) =
  let test ctx =
    let lexbuf = Lexing.from_string s in
    let ast' = prog tokenize lexbuf in
    assert_equal ~cmp:ast_equal expected ast'
                 ~printer:(fun a -> Sexp.to_string (S.sexp_of_exp a))
  in
  test

let assert_parse_succ (s : string) =
  let test ctx =
    try
      let lexbuf = Lexing.from_string s in
      ignore(prog tokenize lexbuf)
    with
    | _ -> assert_failure "parse should've succeeded but failed."
  in
  test

let assert_parse_fail (s : string) =
  let test ctx =
    try
      let lexbuf = Lexing.from_string s in
      ignore(prog tokenize lexbuf);
      assert_failure "parse should've failed but succeeded."
    with _ -> ()
  in
  test

let check_file filename =
  let lines = List.of_enum (File.lines_of filename) in
  let s = String.join "\n" lines in
  let hdr = String.uppercase (List.hd lines) in
  if String.exists hdr "ERROR" then
    assert_parse_fail s
  else
    assert_parse_succ s

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
                 S.Seq([S.Int(dummy, 1)])))
    ;
      "record type declaration" >::
        assert_parse
          "let type myint = { lo: int, mid: int, hi: int} in 1 end"
            (S.Let(dummy,
                   [S.TypeDecl([dummy, "myint",
                               S.RecordTy([{S.fldName= "lo"; S.ty = "int"; S.pos = dummy};
                                           {S.fldName= "mid"; S.ty = "int"; S.pos = dummy};
                                           {S.fldName= "hi"; S.ty = "int"; S.pos = dummy}])])],
                   S.Seq([S.Int(dummy, 1)])))
    ;

      "record type decl [empty fields]" >::
        assert_parse
          "let type myint = {} in 1 end"
          (S.Let(dummy,
                [S.TypeDecl(
                     [dummy, "myint", S.RecordTy([])])],
                S.Seq([S.Int(dummy, 1)])))
    ;

      "array type declaration" >::
        assert_parse
          "let type ary = array of int in 1 end"
          (S.Let(dummy,
                 [S.TypeDecl([dummy, "ary", S.ArrayTy(dummy, "int")])],
                 S.Seq([S.Int(dummy, 1)])))
    ;

      "variable decl" >::
        assert_parse
          "let var x := 12 in x end"
          (S.Let(dummy,
                 [S.VarDecl(dummy, "x", None, S.Int(dummy, 12))],
                 S.Seq([S.Var(dummy, S.VarId("x"))])))
    ;

      "varable decl" >::
        assert_parse
          "let var x : int := 12 in x end"
          (S.Let(dummy,
                 [S.VarDecl(dummy, "x", Some "int", S.Int(dummy, 12))],
                 S.Seq([S.Var(dummy, S.VarId("x"))])))
    ;

      "nil" >::
        assert_parse
          "let type rectype = {name:string, id:int}
               var b:rectype := nil
           in b := nil end"
          (S.Let(dummy, [S.TypeDecl([dummy, "rectype",
                                     S.RecordTy([{S.fldName="name"; S.ty="string";S.pos=dummy};
                                                 {S.fldName="id"; S.ty="int";S.pos=dummy}])]);
                         S.VarDecl(dummy, "b", Some("rectype"), S.Nil(dummy))],
                 S.Seq([S.Assign(dummy, S.VarId("b"), S.Nil(dummy))])))
    ;
      "let function decl" >::
        assert_parse
          "let function g(a:int, b:int) = (a)
               function g1(a:int, b:int):int = (b) in 1 end"
          (S.Let(dummy,
                 [S.FunctionDecl(
                      [(dummy, {S.funName="g"; S.fresult=None; S.fbody=S.Seq([S.Var(dummy, S.VarId("a"))]);
                                S.fparams=[{S.fldName="a"; S.ty="int"; S.pos=dummy};
                                           {S.fldName="b"; S.ty="int"; S.pos=dummy}]});
                       (dummy, {S.funName="g1"; S.fresult=Some "int"; S.fbody=S.Seq([S.Var(dummy, S.VarId("b"))]);
                                S.fparams=[{S.fldName="a"; S.ty="int"; S.pos=dummy};
                                           {S.fldName="b"; S.ty="int"; S.pos=dummy}]})])],
                 S.Seq([S.Int(dummy, 1)])))
    ;

      "array decl" >::
        assert_parse
          "let var arr1 := arrtype1 [10] of 0 in 1 end"
          (S.Let(dummy,
                 [S.VarDecl(dummy, "arr1", None,
                            S.Arr(dummy, "arrtype1", S.Int(dummy, 10), S.Int(dummy, 0)))],
                 S.Seq([S.Int(dummy, 1)])))
    ;

      "let var array" >::
        assert_parse
          "let type arrtype1 = array of int
               var arr1 := arrtype1 [10] of 0
           in arr1[0] := 1; 1 end"
          (S.Let(dummy,
                 [S.TypeDecl([dummy, "arrtype1", S.ArrayTy(dummy, "int")]);
                  S.VarDecl(dummy, "arr1", None,
                            S.Arr(dummy, "arrtype1", S.Int(dummy, 10), S.Int(dummy, 0)))],
                 S.Seq([S.Assign(dummy, S.VarSubscript(S.VarId("arr1"), S.Int(dummy, 0)),
                                 S.Int(dummy, 1)); S.Int(dummy, 1)])))
    ;


      "op1" >::
        assert_parse
          "1 + 2 - 3"
          (S.Op(dummy, S.OpMinus, S.Op(dummy, S.OpPlus, S.Int(dummy, 1), S.Int(dummy, 2)),
                S.Int(dummy, 3)))
    ;

      "op2 (-x desugars to 0 - x)" >::
        assert_parse
          "1 + 2 - -3"
          (S.Op(dummy, S.OpMinus, S.Op(dummy, S.OpPlus, S.Int(dummy, 1), S.Int(dummy, 2)),
                S.Op(dummy, S.OpMinus, S.Int(dummy, 0), S.Int(dummy, 3))))
    ;

      "op precedence" >::
        assert_parse
          "1 + 2 * 3 / 4"
          (S.Op(dummy, S.OpPlus, S.Int(dummy, 1),
                S.Op(dummy, S.OpDiv, S.Op(dummy, S.OpTimes, S.Int(dummy, 2), S.Int(dummy, 3)),
                     S.Int(dummy, 4))))
    ;

      "and" >::
        assert_parse
          "1 + 1 & 2 + 2"
          (S.If(dummy, S.Op(dummy, S.OpPlus, S.Int(dummy, 1), S.Int(dummy, 1)),
                S.Op(dummy, S.OpPlus, S.Int(dummy, 2), S.Int(dummy, 2)), Some (S.Int(dummy, 0))))
    ;

      "or" >::
        assert_parse
          "1 + 1 | 2 + 2"
          (S.If(dummy, S.Op(dummy, S.OpPlus, S.Int(dummy, 1), S.Int(dummy, 1)),
                S.Int(dummy, 1), Some (S.Op(dummy, S.OpPlus, S.Int(dummy, 2), S.Int(dummy, 2)))))
    ;

      "If" >::
        assert_parse
          "if 1 then if 2 then 3 else 4"
          (S.If(dummy, S.Int(dummy, 1),
                S.If(dummy, S.Int(dummy, 2), S.Int(dummy, 3), Some (S.Int(dummy, 4))), None))
    ;

      "seq" >::
        assert_parse
          "(1;2; ())"
          (S.Seq([S.Int(dummy, 1);
                  S.Int(dummy, 2);
                  S.Seq([])]))
    ;

      "call" >::
        assert_parse
          "foo(1,2)"
          (S.Call(dummy, "foo", [S.Int(dummy, 1); S.Int(dummy, 2)]))
    ;

      "while" >::
        assert_parse
          "while 10 >= 5 do (1 + 1; break; ())"
          (S.While(dummy, S.Op(dummy, S.OpGe, S.Int(dummy, 10), S.Int(dummy, 5)),
                   S.Seq([S.Op(dummy, S.OpPlus, S.Int(dummy, 1), S.Int(dummy, 1));
                          S.Break(dummy);
                          S.Seq([])])))
    ;

      "a := b + c" >::
        assert_parse
        "a := b + c"
        (S.Assign(dummy, S.VarId("a"), S.Op(dummy, S.OpPlus, S.Var(dummy, S.VarId("b")), S.Var(dummy, S.VarId("c")))))
    ;

      "lvalue and id shift/reduce" >::
        assert_parse
          "a[0] := 1"
          (S.Assign (dummy, S.VarSubscript(S.VarId("a"), S.Int(dummy, 0)), S.Int(dummy, 1)))
    ;

    ]


let _ =
  run_test_tt_main suite
