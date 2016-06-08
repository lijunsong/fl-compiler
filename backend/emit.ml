(** [emit_instr instr f fm] will emit assembly instructions given by
    [instr]. [f] is used for convert temp to names. [fm] is used to
    generate prolog and epilog. *)
let emit_instr instr f fm =
  let instr_str = List.map (fun i ->
      Selection.format f i) instr in
  let prog = Arch.add_prolog_epilog fm instr_str in
  List.iter print_endline prog


let emit_data frags =
  let data = List.map
      (function
        | Arch.PROC (_) -> failwith "found non-string."
        | Arch.STRING(l, s) -> (l, s)) frags in
  let data_string = List.map (fun (l, s) -> Arch.string l s) data in
  List.iter print_endline data_string
