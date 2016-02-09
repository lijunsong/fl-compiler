module S = Syntax
open Sexplib
open Parse
open Printf
open Batteries

type lang =
  | TIGER of string
  | AST of S.exp
  | IR of Translate.exp
  | EMPTY

let program = ref EMPTY

let load s =
  let contents = Util.file_to_string s in
  program := TIGER(contents)

let load_stdin () =
  let contents = input_all Pervasives.stdin in
  program := TIGER(contents)

let to_ast () =
  match !program with
  | EMPTY -> failwith "laod a tiger program first!"
  | TIGER(t) ->
    let ast = Parse.parse_string t in
    program := AST(ast)
  | AST(t) -> ()
  | IR(_) -> failwith "Cannot convert from IR to AST"

let to_ir () =
  match !program with
  | EMPTY -> failwith "load a tiger program first!"
  | TIGER(t) ->
    let ast = Parse.parse_string t in
    let ir, _ = Semant.trans_prog ast in
    program := IR(ir)
  | AST(ast) ->
    let ir, _ = Semant.trans_prog ast in
    program := IR(ir)
  | IR(_) -> ()

let type_check () =
  match !program with
  | EMPTY -> failwith "Nothing to type check"
  | TIGER(t) ->
    Semant.type_check (Parse.parse_string t)
  | AST(ast) ->
    Semant.type_check ast
  | IR(_) -> failwith "Type checker works only on tiger program or its AST."

let to_canon () =
  to_ir ();
  match !program with
  | IR(ir) ->
  | _ -> failwith "unreachable"

let print () =
  match !program with
  | EMPTY -> failwith "load a tiger program first!"
  | TIGER(t) ->
    print_endline t
  | AST(ast) ->
    let sexp = S.sexp_of_exp ast in
    Sexp.output_hum Pervasives.stdout sexp
  | IR(ir) ->
    let sexp = Translate.sexp_of_exp ir in
    Sexp.output_hum Pervasives.stdout sexp

let specs = [
  ("-stdin", Arg.Unit(load_stdin), "load a tiger program from stdin");
  ("-load", Arg.String(load), "load a tiger program");
  ("-ast", Arg.Unit(to_ast), "convert the program to an AST");
  ("-ir", Arg.Unit(to_ir), "convert the program to ir");
  ("-type-check", Arg.Unit(type_check), "type check the given program (tiger or AST)");
  ("-p", Arg.Unit(print), "print the program");
]

let anno s =
  failwith ("unknown argument: " ^ s)

let usage_msg = "A tiger complier in OCaml."
let _ =
  Arg.parse specs anno usage_msg;
  if !program = EMPTY then
    Arg.usage specs usage_msg
