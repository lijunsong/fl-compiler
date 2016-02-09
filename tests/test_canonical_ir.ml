open OUnit2
open Batteries
open Ir

let assert_linearize ir expect_list =
  let filter_const0 lst =
    List.filter (fun e -> match e with
        | EXP(CONST(0)) -> false
        | _ -> true) lst
  in
  let printer stmts : string =
    String.concat "\n" (List.map Ir.stmt_to_string stmts)
  in
  let test ctx =
    let result = Canon.linearize ir |> filter_const0 in
    let expect = filter_const0 expect_list in
    assert_equal ~printer:printer expect result
  in
  test

let suite =
  "Test Canonical IR Transformation" >::: [
    "ESEQ" >::
    assert_linearize
      (EXP(ESEQ(EXP(CONST(1)), ESEQ(EXP(CONST(2)), CONST(3)))))
      [EXP(CONST(1)); EXP(CONST(2)); EXP(CONST(3))]
    ;

    "ESEQ2" >::
    assert_linearize
      (EXP(ESEQ(EXP(ESEQ(EXP(CONST(1)), CONST(2))), CONST(3))))
      [EXP(CONST(1)); EXP(CONST(2)); EXP(CONST(3))]
    ;

    "BINOP" >::
    assert_linearize
      (EXP(BINOP(PLUS, ESEQ(EXP(CONST(100)), CONST(1)), CONST(2))))
      [EXP(CONST(100)); EXP(BINOP(PLUS, CONST(1), CONST(2)))]
    ;

    "BINOP2" >::
    assert_linearize
      (EXP(BINOP(PLUS, CONST(1), ESEQ(EXP(CONST(100)), CONST(2)))))
      [EXP(CONST(100)); EXP(BINOP(PLUS, CONST(1), CONST(2)))]
    ;

    "MEM" >::
    assert_linearize
      (EXP(MEM(ESEQ(EXP(CONST(1)), CONST(2)))))
      [EXP(CONST(1)); EXP(MEM(CONST(2)))]
    ;

    "JUMP" >::
    let labels = [Temp.new_label(); Temp.new_label()] in
    assert_linearize
      (JUMP(ESEQ(EXP(CONST(1)), CONST(2)), labels))
      [EXP(CONST(1)); JUMP(CONST(2), labels)]
      ;
  ]

let _ =
  run_test_tt_main suite
