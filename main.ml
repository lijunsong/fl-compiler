module S = Syntax
open Sexplib
open Parse
open Printf
open Batteries

type lang =
  | TIGER of string
  | AST of S.exp
  | IR of Translate.exp
  | CANON of Ir.stmt list
  | BLOCKS of Ir.stmt list list * Temp.label
  | TRACE of Ir.stmt list
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
  | _ -> failwith "Cannot convert from IR to what you asked."

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
  | _ -> ()

let type_check () =
  match !program with
  | EMPTY -> failwith "Nothing to type check"
  | TIGER(t) ->
    Semant.type_check (Parse.parse_string t)
  | AST(ast) ->
    Semant.type_check ast
  | _ ->
    failwith "Type checker works only on tiger program or its AST."

let to_canon () =
  to_ir ();
  match !program with
  | IR(ir) ->
     program := CANON(Canon.linearize (Translate.unNx ir))
  | _ -> failwith "unreachable"

let to_blocks () =
  to_canon ();
  match !program with
  | CANON(ir) ->
     let bb, label = Canon.basic_blocks ir in
     program := BLOCKS(bb, label)
  | _ -> failwith "unreachable"

let to_trace () =
  to_blocks ();
  match !program with
  | BLOCKS(ir, label) ->
     program := TRACE(Canon.trace_schedule (ir, label))
  | _ -> failwith "unreachable"

let print () =
  let print_ir_list list =
     List.iter (fun stmt ->
         let sexp = Ir.sexp_of_stmt stmt in
         Sexp.output_hum Pervasives.stdout sexp;
         print_string "\n";
       ) list
  in
  match !program with
  | EMPTY -> failwith "load a tiger program first!"
  | TIGER(t) ->
    print_endline t
  | AST(ast) ->
    let sexp = S.sexp_of_exp ast in
    Sexp.output_hum Pervasives.stdout sexp
  | CANON(list) ->
     print_ir_list list
  | BLOCKS(ir, label) ->
     List.iter (fun lst -> print_ir_list lst;
                        print_string "\n")
               ir
  | TRACE(list) ->
     print_ir_list list
  | IR(ir) ->
    let sexp = Translate.sexp_of_exp ir in
    Sexp.output_hum Pervasives.stdout sexp

let specs = [
  ("-stdin", Arg.Unit(load_stdin), "load a tiger program from stdin");
  ("-load", Arg.String(load), "load a tiger program");
  ("-ast", Arg.Unit(to_ast), "convert the program to an AST");
  ("-ir", Arg.Unit(to_ir), "convert the program to ir");
  ("-canon", Arg.Unit(to_canon), "convert the program to Canonical IR");
  ("-trace", Arg.Unit(to_trace), "convert the program to Traced IR");
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
