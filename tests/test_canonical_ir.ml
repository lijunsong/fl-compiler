open OUnit2
open Batteries
open Ir

let filter_const0 lst =
  List.filter (fun e -> match e with
      | EXP(CONST(0)) -> false
      | _ -> true) lst

let assert_linearize (ir : Ir.stmt) (expect_list : Ir.stmt list) =
  let printer stmts : string =
    String.concat "\n" (List.map Ir.stmt_to_string stmts)
  in
  let test ctx =
    let result = Canon.linearize ir |> filter_const0 in
    let expect = filter_const0 expect_list in
    assert_equal ~cmp:Utils.ir_eq ~printer:printer expect result
  in
  test

let suite =
  "Test Canonical IR Transformation" >::: [
    "ESEQ" >::
    assert_linearize
      (EXP(ESEQ(EXP(CONST(1)), ESEQ(EXP(CONST(2)), CONST(3)))))
      [EXP(CONST(1)); EXP(CONST(2)); EXP(CONST(3))]
    ;

    "ESEQ (ESEQ at left branch)" >::
    assert_linearize
      (EXP(ESEQ(EXP(ESEQ(EXP(CONST(1)), CONST(2))), CONST(3))))
      [EXP(CONST(1)); EXP(CONST(2)); EXP(CONST(3))]
    ;

    "BINOP (ESEQ at left branch)" >::
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
    (let labels : Temp.label list = [Temp.new_label(); Temp.new_label()] in
     assert_linearize
       (JUMP(ESEQ(EXP(CONST(1)), CONST(2)), labels))
       [EXP(CONST(1)); JUMP(CONST(2), labels)])
    ;


    "MOVE from ESEQ" >::
    (let temp : Ir.exp = TEMP(Temp.new_temp()) in
     assert_linearize
       (MOVE(temp, ESEQ(EXP(CONST(1)), CONST(2))))
       [EXP(CONST(1)); MOVE(temp, CONST(2))])
    ;

    "MOVE to ESEQ" >::
    assert_linearize
      (MOVE(MEM(ESEQ(EXP(CONST(1)), CONST(2))), CONST(3)))
      [EXP(CONST(1)); MOVE(MEM(CONST(2)), CONST(3))]
    ;

    "MOVE complex" >::
    assert_linearize
      (MOVE(MEM(ESEQ(EXP(CONST(1)), CONST(2))),
            ESEQ(EXP(CONST(3)), CONST(4))))
      [EXP(CONST(1)); EXP(CONST(3)); MOVE(MEM(CONST(2)), CONST(4))]
    ;

    "EXP ESEQ" >::
    assert_linearize
      (EXP(ESEQ(EXP(CONST(1)), CONST(2))))
      [EXP(CONST(1)); EXP(CONST(2))]
    ;

    "EXP(CALL)" >::
    (let temp = TEMP(Temp.new_temp()) in
    assert_linearize
      (EXP(CALL(ESEQ(EXP(CONST(1)), CONST(2)),
                [CONST(3); CONST(4)])))
      [EXP(CONST(1));
       MOVE(temp, CALL(CONST(2),
                       [CONST(3); CONST(4)]));
       EXP(temp)]
    )
    ;

    "ESEQ IN CALL" >::
    (let temp = TEMP(Temp.new_temp()) in
     assert_linearize
       (EXP(CALL(CONST(1), [CONST(2);
                            ESEQ(EXP(CONST(3)), CONST(4));
                            ESEQ(EXP(CONST(5)), CONST(6))])))
       [EXP(CONST(3)); EXP(CONST(5));
        MOVE(temp, CALL(CONST(1), [CONST(2); CONST(4); CONST(6)]));
        EXP(temp)]
    )
    ;

    "SEQ form 1" >::
    assert_linearize
      (SEQ(SEQ(EXP(CONST(1)), EXP(CONST(2))), EXP(CONST(3))))
      [EXP(CONST(1)); EXP(CONST(2)); EXP(CONST(3))]
    ;

    "SEQ form 2" >::
    assert_linearize
      (SEQ(EXP(CONST(1)), SEQ(EXP(CONST(2)), SEQ(EXP(CONST(3)),
                                                 EXP(CONST(4))))))
      [EXP(CONST(1)); EXP(CONST(2)); EXP(CONST(3)); EXP(CONST(4))]
    ;

    "raise CALL" >::
    (
      (* This test uses ir_eq to allow different temps serving the
      * same purpose. *)
      let temp1 = TEMP(Temp.new_temp()) in
      let temp2 = TEMP(Temp.new_temp()) in
      let temp3 = TEMP(Temp.new_temp()) in
      let expected = [
        MOVE(temp1, CALL(CONST(3),[]));
        MOVE(temp2, CALL(CONST(6),[]));
        MOVE(temp3, CALL(CONST(2), [temp1; CONST(5); temp2]));
        MOVE(MEM(CONST(1)), temp3);
      ] in
      assert_linearize
        (MOVE(MEM(CONST(1)),
                  CALL(CONST(2), [CALL(CONST(3), []);
                                  CONST(5);
                                  CALL(CONST(6), [])])))
        expected
    )
    ;
  ]

let _ =
  run_test_tt_main suite
