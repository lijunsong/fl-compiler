module S = Syntax
open Parse
open Printf
open Batteries

type linear = Ir.stmt list

type basicblocks = Ir.stmt list list

type string_frags = Translate.frag list

type linear_proc = linear * Arch.frame

type bb_proc = Basic_block.t list * Temp.label * Arch.frame

type assem_proc = Assem.instr list * Arch.frame

type lang =
  | TIGER of string
  | AST of S.exp
  | IR of Translate.frag list
  | CANON of linear_proc list * string_frags
  | BLOCKS of bb_proc list * string_frags
  | TRACE of linear_proc list * string_frags
  | INSTR_SELECT of assem_proc list * string_frags
  | FLOW of Flow.flowgraph list
  (** FLOW only transits to INTERFERENCE. It can't transit to final codegen *)
  | INTERFERENCE of Liveness.igraph list
  (** FLOW only transits to INTERFERENCE. It can't transit to final codegen *)
  | REGISTER_ALLOC of (assem_proc * Register_allocation.allocation) list * string_frags
  (** Register_alloc will generate instructions. All temporaries are
      maped in allocation *)
  | EMPTY

let program = ref EMPTY

let print_lang lang =
  let print_ir_list list =
    List.iter (fun stmt -> print_endline (Ir.stmt_to_string stmt)) list
  in
  let print_string_frags list =
    print_string "strings: \n";
    List.iter (fun f -> Translate.frag_to_string f |> print_endline) list
  in
  match lang with
  | EMPTY -> failwith "load a tiger program first!"
  | TIGER(t) ->
    print_endline t
  | AST(ast) ->
    print_endline (S.exp_to_string ast)
  | CANON(proc_list, strs) ->
    print_string "programs:\n";
    List.iter (fun (ir_list, fm) ->
        print_ir_list ir_list;
        print_string "frames: \n";
        Arch.debug_dump fm) proc_list;
    print_string_frags strs

  | BLOCKS(bb_proc_list, label) ->
    List.iter (fun (bbs,l,fm) ->
        Arch.debug_dump fm;
        Basic_block.basic_blocks_to_doc bbs |> Pprint.print_doc;
        print_string ("label: " ^ (Temp.label_to_string l) ^ "\n")
      ) bb_proc_list
  | TRACE(proc_list, strs) ->
    List.iter (fun (ir_list, fm) ->
        print_endline "----- frame -----";
        Arch.debug_dump fm;
        print_ir_list ir_list;
        print_endline "----- frame end -----"
      ) proc_list;
    print_string_frags strs
  | IR(ir_list) ->
    List.iter (fun ir -> Translate.frag_to_string ir |> print_endline)
      ir_list
  | INSTR_SELECT(proc_list, str_frags) ->
    (* At this stage, only print machine register name when we know it. *)
    let get_register_name t = match Arch.register_of_temp t with
      | None -> Temp.temp_to_string t
      | Some (reg) -> reg
    in
    let () = List.iter (fun (instrs, fm) ->
        Emit.emit_instr instrs get_register_name fm)
        proc_list in
    Emit.emit_data str_frags


  | FLOW(graphs) ->
    let str_list = List.map Flow.to_string graphs in
    let str = String.concat "------\n" str_list in
    print_endline str
  | INTERFERENCE(igraphs) ->
    let str_list = List.map Liveness.to_string igraphs in
    let str = String.concat "------\n" str_list in
    print_endline str
  | REGISTER_ALLOC (allocs, str_frags) ->
    let () = List.iter (fun ((instrs, fm),alloc) ->
        let get_register_name tmp =
          try Temp.TempMap.find tmp alloc
          with _ -> failwith (sprintf "temp %s is not assigned a register."
                                (Temp.temp_to_string tmp))
        in
        Emit.emit_instr instrs get_register_name fm
      ) allocs in
    Emit.emit_data str_frags

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
           | Arch.PROC (_) -> true
           | _ -> false) frags in
    let canon =
      List.map (fun frag -> match frag with
          | Arch.PROC (ir, fm) -> Canon.linearize ir, fm
          | Arch.STRING (_) -> failwith "unreachable"
        ) progs in
    program := CANON(canon, strs)
  | _ -> failwith "Can't convert to Cannonical IR"

let to_blocks () =
  to_canon ();
  match !program with
  | CANON(ir_list, strs) ->
    let bbs = List.map (fun (ir,fm) ->
        let bb, l = Basic_block.basic_blocks ir in
        bb, l, fm) ir_list in
    program := BLOCKS(bbs, strs)
  | _ -> failwith "Can't convert to blocks"

let to_trace () =
  to_blocks ();
  match !program with
  | BLOCKS(procs, strs) ->
    let traced = List.map (fun (bbs,l,fm) ->
        let list = Trace.trace_schedule (bbs, l) in
        list, fm) procs in
    program := TRACE(traced, strs)
  | _ -> failwith "Can't convert to trace"

let to_assem () =
  to_trace ();
  match !program with
  | TRACE (procs, str_frags) ->
    let res = List.map (fun (ir, frame) ->
        let seq = Ir.seq ir in
        Selection.select_instr frame seq, frame) procs in
    program := INSTR_SELECT(res, str_frags);
  | _ -> failwith "Can't convert to assemly"

let to_regalloc () =
  to_assem();
  match !program with
  | INSTR_SELECT (assems, str_frags) ->
    program := REGISTER_ALLOC(
        List.map (fun (instrs, frame) ->
            let instrs', alloc = Register_allocation.alloc instrs in
            (instrs', frame), alloc) assems,
        str_frags)
  | _ -> failwith "Can't go back to previous compilation process."

let to_flowgraph () =
  to_assem();
  match !program with
  | INSTR_SELECT (assms, str_frags) ->
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
  ("-canon", Arg.Unit(to_canon), "Remove ESEQ and raise CALL");
  ("-basicblock", Arg.Unit(to_blocks), "Generate basic blocks");
  ("-trace", Arg.Unit(to_trace), "Preparing IR for instruction selection");
  ("-select-instr", Arg.Unit(to_assem), "instruction selection");
  ("-codegen", Arg.Unit(to_regalloc), "convert the program to assembly Lang with register allocation (Sparc for now)");
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
