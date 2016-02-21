open Utils
open OUnit
open Batteries

let newl () = Ir.LABEL(Temp.new_label())

let assert_ir_eq list1 list2 =
  (fun ctx ->
     assert_equal
       true
       (ir_eq list1 list2))


let assert_ir_neq list1 list2 =
  (fun ctx ->
     assert_equal
       false
       (ir_eq list1 list2))

let suite =
  "Test testing utils" >::: [
    "Ir eq test1" >::
    (assert_ir_eq
       [newl(); newl()]
       [newl(); newl()])
    ;

    "Ir eq test2" >::
    (let l1 = newl() in
     let l2 = newl() in
     assert_ir_eq
       [l1; newl(); l1]
       [l2; newl(); l2])
    ;

    "Ir eq test3 (different labels map to the same label)" >::
    (let l1 = newl() in
      assert_ir_neq
      [newl(); newl()]
      [l1; l1])
    ;

    "Ir eq test4 (The same label maps to different labels)" >::
    (let l1 = newl () in
     let l2 = newl () in
     assert_ir_neq
       [l1; l2; l1]
       [newl(); newl(); newl()])
    ;

    "Ir eq test (same label)" >::
    (let l1 = newl() in
     let l2 = newl() in
     assert_ir_eq
       [l1; l2; l1]
       [l1; l2; l1])
    ;
  ]


let _ =
  run_test_tt_main suite
