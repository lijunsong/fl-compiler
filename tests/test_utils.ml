open Utils
open Batteries

let newl () = Temp.new_label()

let assert_ir_eq list1 list2 =
  (fun ctx ->
     assert_equal
       (ir_eq list1 list2)
       true)

let suite =
  "Test testing utils" >::: [
    "Ir eq test1" >::
    (ir_test
       [newl(); newl()]
       [newl(); newl()])
    ;

    "Ir eq test2" >::
    (let l1 = newl() in
     let l2 = newl() in
     ir_test 
       [l1; newl(); l1]
       [l2; newl(); l2])
    ;

    "Ir eq test3" >::
    (ir_test [])


let _ =
  run_test_tt_main suite
