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

type assem_proc = Assem.instr list * Translate.F.frame

type lang =
  | TIGER of string
  | AST of S.exp
  | IR of Translate.frag list
  | CANON of linear_proc list * string_frags
  | BLOCKS of bb_proc list * string_frags
  | TRACE of linear_proc list * string_frags
  | ASSEM of assem_proc list * string_frags
  | FLOW of Flow.flowgraph list
  (** FLOW only transits to INTERFERENCE. It can't transit to final codegen *)
  | INTERFERENCE of Liveness.igraph list
  (** FLOW only transits to INTERFERENCE. It can't transit to final codegen *)
  | REGISTER_ALLOC of (assem_proc * Register_allocation.allocation) list * string_frags
  (** Register_alloc will generate instructions. All temporaries are
      maped in allocation *)
  | EMPTY

let program = ref EMPTY

let assem_proc_to_string get_register_name (instr_list, fm) =
  let body = List.map (fun instr ->
      Codegen.format get_register_name instr) instr_list in
  let all = Translate.F.proc_entry_exit3 fm body in
  all

let print_lang lang =
  let print_ir_list list =
    List.iter (fun stmt ->
        let sexp = Ir.sexp_of_stmt stmt in
        Sexp.output_hum Pervasives.stdout sexp;
        print_string "\n";
      ) list
  in
  let print_string_frags list =
    print_string "strings: \n";
    List.iter (fun s ->
        let sexp = Translate.sexp_of_frag s in
        Sexp.output_hum Pervasives.stdout sexp;
        print_string "\n";) list
  in
  match lang with
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
    print_string_frags strs

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
        print_endline "----- frame -----";
        Translate.F.debug_dump fm;
        print_ir_list ir_list;
        print_endline "----- frame end -----"
      ) proc_list;
    print_string_frags strs
  | IR(ir_list) ->
    List.iter (fun ir ->
        let sexp = Translate.sexp_of_frag ir in
        Sexp.output_hum Pervasives.stdout sexp;
        print_string "\n")
      ir_list
  | ASSEM(proc_list, str_frags) ->
    (* At this stage, only print machine register name when we know it. *)
    let get_register_name t = match Translate.F.get_register_name t with
      | None -> Temp.temp_to_string t
      | Some (reg) -> reg
    in
    List.iter (fun p -> let s = assem_proc_to_string get_register_name p in
                List.iter print_endline s) proc_list

  | FLOW(graphs) ->
    let str_list = List.map Flow.to_string graphs in
    let str = String.concat "------\n" str_list in
    print_endline str
  | INTERFERENCE(igraphs) ->
    let str_list = List.map Liveness.to_string igraphs in
    let str = String.concat "------\n" str_list in
    print_endline str
  | REGISTER_ALLOC (allocs, str_frags) ->
    let str_list = List.map (fun ((instrs, fm),alloc) ->
        let get_register_name tmp =
          try Temp.TempMap.find tmp alloc
          with _ -> failwith (sprintf "temp %s is not assigned a register." (Temp.temp_to_string tmp))
        in
        let assem = String.concat "\n" (assem_proc_to_string get_register_name (instrs, fm)) in
        let assem' = if !(Debug.debug) then
            let alloc_str = Color.allocation_to_string alloc in
            assem ^ "\n" ^ alloc_str
          else
            assem in
        assem'
      ) allocs in
    let text_header = [".section \".text\"";
                       ".align 16"] |> String.concat "\n" in
    let str = String.concat "\n" str_list in
    (* emit text header and the code text *)
    let () = print_endline text_header in
    let () = print_endline str in
    (* emit the string *)
    let frags = List.map (fun frag -> match frag with
        | Translate.F.PROC(_) -> failwith "proc found in string frags."
        | Translate.F.STRING(l, s) -> (l, s)) str_frags in
    let data = Codegen.codegen_data frags in
    print_endline data


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
  | _ -> failwith "Can't convert to Cannonical IR"

let to_blocks () =
  to_canon ();
  match !program with
  | CANON(ir_list, strs) ->
    let bbs = List.map (fun (ir,fm) ->
        let bb, l = Canon.basic_blocks ir in
        bb, l, fm) ir_list in
    program := BLOCKS(bbs, strs)
  | _ -> failwith "Can't convert to blocks"

let to_trace () =
  to_blocks ();
  match !program with
  | BLOCKS(bbs, strs) ->
    let traced = List.map (fun (bb,l,fm) ->
        let list = Canon.trace_schedule (bb, l) in
        list, fm) bbs in
    program := TRACE(traced, strs)
  | _ -> failwith "Can't convert to trace"

let to_assem () =
  to_trace ();
  match !program with
  | TRACE (procs, str_frags) ->
    let res = List.map (fun (ir, frame) ->
        let seq = Ir.seq ir in
        Codegen.codegen frame seq, frame) procs in
    program := ASSEM(res, str_frags);
  | _ -> failwith "Can't convert to assemly"

let to_regalloc () =
  to_assem();
  match !program with
  | ASSEM (assems, str_frags) ->
    program := REGISTER_ALLOC(
        List.map (fun (instrs, frame) ->
            let instrs', alloc = Register_allocation.alloc instrs in
            (instrs', frame), alloc) assems,
        str_frags)
  | _ -> failwith "Can't go back to previous compilation process."

let to_flowgraph () =
  to_assem();
  match !program with
  | ASSEM (assms, str_frags) ->
    program := FLOW (List.map (fun (ass, _) -> Flow.instrs2graph ass) assms)
  | _ -> failwith "Can't convert to a flowgraph."

let to_igraph () =
  to_flowgraph();
  match !program with
  | FLOW (fgraph) ->
    program := INTERFERENCE(List.map Liveness.flow2igraph fgraph)
  | _ -> failwith "Can't convert to an interference graph"

let print () = print_lang !program

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
