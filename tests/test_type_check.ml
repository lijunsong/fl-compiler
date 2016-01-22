open OUnit2
open Parse
open Batteries

let assert_pass (s : string) =
  let test ctx =
    let ast = Parse.parse_string s in
    Semant.transProg (ast)
  in
  test

let assert_fail (s : string) =
  let test ctx =
    let ast = Parse.parse_string s in
    try
      ignore(Semant.transProg (ast));
      assert_failure "unexpected pass."
    with
    | Failure (msg) -> assert_failure "Internal Error"
    | Semant.TypeError (_) -> ()
    | Semant.UndefinedError (_) -> ()
  in
  test

let check_file filename =
  let lines = List.of_enum (File.lines_of filename) in
  let s = String.join "\n" lines in
  let hdr = String.uppercase (List.hd lines) in
  if String.exists hdr "ERROR" || String.exists hdr "ILLEGAL" then
    assert_fail s
  else
    assert_pass s

let test_files () : string list =
  let dir = "tests/type_checker_tests/" in (** run in project root *)
  let handler = Unix.opendir dir in
  let rec read_all_files result =
    try
      let f = Unix.readdir handler in
      if String.ends_with f ".tig" then
        read_all_files ((dir ^ f) :: result)
      else read_all_files result
    with
      End_of_file -> result
  in
  read_all_files []

let get_external_tests () =
  List.map (fun file ->
      file >:: check_file file)
           (test_files())

let suite =
  "Type Checker" >::: [
      "undef field type" >::
        assert_fail
          "/* error: undef field type */ let type a = {x : int, y : nonexists} in 1 end"
    ;
      "undef array of" >::
        assert_fail
          "/* error: undef array of */ let type a = array of nonexists in 1 end"
    ;
    ] @ (get_external_tests())

let _ =
  run_test_tt_main suite
