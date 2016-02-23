open Utils
open OUnit
open Ir
open Batteries

let newl () = LABEL(Temp.new_label())

let printer s : string =
  (String.concat "\n" (List.map Ir.stmt_to_string s))

let assert_basic_block ir expected =
  (fun ctx ->
     let result, _ = Canon.basic_blocks ir in
     let result' = List.flatten result in
     assert_equal ~cmp:ir_eq ~printer:printer
       expected result')

let assert_trace ir expected =
  (fun ctx ->
     let result =
       Canon.basic_blocks ir
       |> Canon.trace_schedule in
     assert_equal
       ~cmp:ir_eq
       ~printer:printer
       expected
       result)


let assert_ir_neq ir expected =
  (fun ctx ->
     let result =
       Canon.basic_blocks ir
       |> Canon.trace_schedule in
     assert_equal
       ~cmp:ir_eq
       ~printer:printer
       expected
       result)

let suite =
  "Test Basic Blocks and Traces" >::: [
    "Test basic block: add label" >::
    (let l1 = Temp.new_label() in
     let exitl = Temp.new_label() in
     assert_basic_block
       [JUMP(NAME(l1), [l1]);
        EXP(CONST(0)); (*dead*)
        LABEL(l1);
        EXP(CONST(1));]
       [newl();
        JUMP(NAME(l1), [l1]);
        newl();
        EXP(CONST(0)); (*dead*)
        JUMP(NAME(l1), [l1]);
        LABEL(l1);
        EXP(CONST(1));
        JUMP(NAME(exitl), [exitl])]
       )
    ;

    "Test basic block1: append jump" >::
    (let l1 = Temp.new_label() in
     let l2 = Temp.new_label() in
     let l3 = Temp.new_label() in
     let exitl = Temp.new_label() in
     assert_basic_block
       [LABEL(l1); LABEL(l2); LABEL(l3)]
       [LABEL(l1);
        JUMP(NAME(l2), [l2]);
        LABEL(l2);
        JUMP(NAME(l3), [l3]);
        LABEL(l3);
        JUMP(NAME(exitl), [exitl])
       ])
    ;

    "Test basic blocks: no need to modify" >::
    (let t = Temp.new_label() in
     let f = Temp.new_label() in
     let fi = Temp.new_label() in
     let exitl = Temp.new_label() in
     assert_basic_block
       [CJUMP(EQ, CONST(1), CONST(2), t, f);
        LABEL(t);
        EXP(CONST(3));
        JUMP(NAME(fi), [fi]);
        LABEL(f);
        EXP(CONST(4));
        JUMP(NAME(fi), [fi]);
        LABEL(fi);]
       [newl();
        CJUMP(EQ, CONST(1), CONST(2), t, f);
        LABEL(t);
        EXP(CONST(3));
        JUMP(NAME(fi), [fi]);
        LABEL(f);
        EXP(CONST(4));
        JUMP(NAME(fi), [fi]);
        LABEL(fi);
        JUMP(NAME(exitl), [exitl])
       ])
     ;

    "Test trace: move false label" >::
    (let t = Temp.new_label() in
     let f = Temp.new_label() in
     let fi = Temp.new_label() in
     let exitl = Temp.new_label ~prefix:"exit" () in
     assert_trace
       [newl();
        CJUMP(EQ, CONST(1), CONST(2), t, f);
        LABEL(t);
        EXP(CONST(3));
        JUMP(NAME(fi), [fi]);
        LABEL(f);
        EXP(CONST(4));
        JUMP(NAME(fi), [fi]);
        LABEL(fi);]
       [newl();
        CJUMP(EQ, CONST(1), CONST(2), t, f);
        LABEL(f);
        EXP(CONST(4));
        JUMP(NAME(fi), [fi]);
        LABEL(t);
        EXP(CONST(3));
        JUMP(NAME(fi), [fi]);
        LABEL(fi);
        JUMP(NAME(exitl), [exitl]); (* jump to exit *)
        LABEL(exitl) (* add exit label *)])
     ;

    "Test trace: move label to closest jump" >::
    (let t = Temp.new_label ~prefix:"t" () in
     let f = Temp.new_label ~prefix:"f" () in
     let fi = Temp.new_label ~prefix:"fi" () in
     let exitl = Temp.new_label ~prefix:"exit" () in
     assert_trace
       [newl();
        JUMP(NAME(f), [f]);
        LABEL(t); (* This block will be eliminated. *)
        EXP(CONST(3));
        JUMP(NAME(fi), [fi]);
        LABEL(f);
        EXP(CONST(4));
        JUMP(NAME(fi), [fi]);
        LABEL(fi);]
       [newl();
        JUMP(NAME(f), [f]);
        LABEL(f);
        EXP(CONST(4));
        JUMP(NAME(fi), [fi]);
        LABEL(fi);
        JUMP(NAME(exitl), [exitl]); (* appended jump *)
        LABEL(exitl) (* add exit label *)])
  ]


let _ =
  run_test_tt_main suite
