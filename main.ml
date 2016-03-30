module S = Syntax
open Sexplib
open Parse
open Printf
open Batteries

type linear = Ir.stmt list

type basicblocks = Ir.stmt list list

type string_frags = Translate.frag list

type linear_proc = linear * Translate.F.frame

type bb_proc = linear list * Temp.label * Translate.F.frame

type lang =
  | TIGER of string
  | AST of S.exp
  | IR of Translate.frag list
  | CANON of linear_proc list * string_frags
  | BLOCKS of bb_proc list * string_frags
  | TRACE of linear_proc list * string_frags
  | ASSEM of Assem.instr list list
  (** each function will generate a list of instr *)
  | FLOW of Flow.flowgraph list
  | INTERFERENCE of Liveness.igraph list
  | REGISTER_ALLOC of (Assem.instr list * Register_allocation.allocation) list
  (** Register_alloc will generate instructions and all temporaries
      are maped in allocation *)
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
    let frags = Semant.trans_prog ast in
    program := IR(frags)
  | AST(ast) ->
    let frags = Semant.trans_prog ast in
    program := IR(frags)
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
  | IR(frags) ->
    let progs, strs = List.partition
        (fun frag -> match frag with
           | Translate.F.PROC (_) -> true
           | _ -> false) frags in
    let canon =
      List.map (fun frag -> match frag with
          | Translate.F.PROC (ir, fm) -> Canon.linearize ir, fm
          | Translate.F.STRING (_) -> failwith "unreachable"
        ) progs in
    program := CANON(canon, strs)
  | _ -> failwith "unreachable"

let to_blocks () =
  to_canon ();
  match !program with
  | CANON(ir_list, strs) ->
    let bbs = List.map (fun (ir,fm) ->
        let bb, l = Canon.basic_blocks ir in
        bb, l, fm) ir_list in
    program := BLOCKS(bbs, strs)
  | _ -> failwith "unreachable"

let to_trace () =
  to_blocks ();
  match !program with
  | BLOCKS(bbs, strs) ->
    let traced = List.map (fun (bb,l,fm) ->
        let list = Canon.trace_schedule (bb, l) in
        list, fm) bbs in
    program := TRACE(traced, strs)
  | _ -> failwith "unreachable"

let to_assem () =
  to_trace ();
  match !program with
  | TRACE (linear_list, str) ->
    let res = List.map (fun (ir, frame) ->
        let seq = Ir.seq ir in
        Codegen.codegen frame seq) linear_list in
    program := ASSEM(res)
  | _ -> failwith "unreachable"

let to_regalloc () =
  to_assem();
  match !program with
  | ASSEM (assems) ->
    program := REGISTER_ALLOC(
        List.map Register_allocation.alloc assems
      )
  | _ -> failwith "Can't go back to previous compilation process."

let to_flowgraph () =
  to_assem();
  match !program with
  | ASSEM (assms) ->
    program := FLOW (List.map Flow.instrs2graph assms)
  | _ -> failwith "unreachable"

let to_igraph () =
  to_flowgraph();
  match !program with
  | FLOW (fgraph) ->
    program := INTERFERENCE(List.map Liveness.flow2igraph fgraph)
  | _ -> failwith "unreachable"


let print () =
  let print_ir_list list =
    List.iter (fun stmt ->
        let sexp = Ir.sexp_of_stmt stmt in
        Sexp.output_hum Pervasives.stdout sexp;
        print_string "\n";
      ) list
  in
  let print_str_list list =
    print_string "strings: \n";
    List.iter (fun s ->
        let sexp = Translate.sexp_of_frag s in
        Sexp.output_hum Pervasives.stdout sexp;
        print_string "\n";) list
  in
  match !program with
  | EMPTY -> failwith "load a tiger program first!"
  | TIGER(t) ->
    print_endline t
  | AST(ast) ->
    let sexp = S.sexp_of_exp ast in
    Sexp.output_hum Pervasives.stdout sexp
  | CANON(proc_list, strs) ->
    print_string "programs:\n";
    List.iter (fun (ir_list, fm) ->
        print_ir_list ir_list;
        print_string "frames: \n";
        Translate.F.debug_dump fm) proc_list;
    print_str_list strs

  | BLOCKS(bb_proc_list, label) ->
    List.iter (fun (bb,l,fm) ->
        Translate.F.debug_dump fm;
        print_string ("label: " ^ (Temp.label_to_string l) ^ "\n");
        List.iter (fun lst ->
            print_string "block:\n";
            print_ir_list lst) bb;
      ) bb_proc_list
  | TRACE(proc_list, strs) ->
    List.iter (fun (ir_list, fm) ->
        print_ir_list ir_list;
        Translate.F.debug_dump fm) proc_list;
    print_str_list strs
  | IR(ir_list) ->
    List.iter (fun ir ->
        let sexp = Translate.sexp_of_frag ir in
        Sexp.output_hum Pervasives.stdout sexp;
        print_string "\n")
      ir_list
  | ASSEM(instr_list) ->
    List.iter (fun instr_list ->
        List.iter (fun instr ->
            let s = Codegen.format Translate.F.get_register_name instr in
            print_endline s) instr_list)
      instr_list
  | FLOW(graphs) ->
    let str_list = List.map Flow.to_string graphs in
    let str = String.concat "------\n" str_list in
    print_endline str
  | INTERFERENCE(igraphs) ->
    let str_list = List.map Liveness.to_string igraphs in
    let str = String.concat "------\n" str_list in
    print_endline str
  | REGISTER_ALLOC (allocs) ->
    let str_list = List.map (fun (instrs,alloc) ->
        let alloc_str = Color.allocation_to_string alloc in
        let get_register_name tmp = Temp.TempMap.find tmp alloc in
        let instrs_str = List.map (fun instr ->
            let instr_str = Codegen.format get_register_name instr in
            instr_str
          ) instrs
                         |> String.concat "\n" in
        "---- instruction ----\n" ^ instrs_str ^ "\n---allocation---\n"
        ^ alloc_str ^ "---end---\n"
      ) allocs in
    let str = String.concat "------\n" str_list in
    print_endline str


let specs = [
  ("-stdin", Arg.Unit(load_stdin), "load a tiger program from stdin");
  ("-debug", Arg.Set(Debug.debug), "open debug flag");
  ("-load", Arg.String(load), "load a tiger program");
  ("-ast", Arg.Unit(to_ast), "convert the program to an AST");
  ("-ir", Arg.Unit(to_ir), "convert the program to ir");
  ("-canon", Arg.Unit(to_canon), "convert the program to Canonical IR");
  ("-basicblock", Arg.Unit(to_blocks), "convert the program to basic blocks");
  ("-trace", Arg.Unit(to_trace), "convert the program to Traced IR");
  ("-codegen0", Arg.Unit(to_assem), "convert the program to assembly Lang without register allocation (Sparc for now)");
  ("-codegen1", Arg.Unit(to_regalloc), "convert the program to assembly Lang with register allocation (Sparc for now)");
  ("-flowgraph", Arg.Unit(to_flowgraph), "generate flow graph of the program");
  ("-igraph", Arg.Unit(to_igraph), "generate interference graph of the program");
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
