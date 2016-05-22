open Symbol

type symbol = Symbol.t

type pos_t = Pos.t

type exp =
  | String of pos_t * string
  | Int of pos_t * int
  | Nil of pos_t
  | Var of pos_t * var
  | Op of pos_t * op * exp * exp
  (** op * left * right *)
  | Assign of pos_t * var * exp
  | Call of pos_t * symbol * exp list
  | Record of pos_t * symbol * (pos_t * symbol * exp) list
  (** construct a record value: type * fields *)
  | Seq of pos_t * exp list
  (** This construct does not need pos_t *)
  | If of pos_t * exp * exp * exp option
  (** if test then else *)
  | While of pos_t * exp * exp
  (** while test body *)
  | For of pos_t * symbol * exp * exp * exp
  (** var * lo * hi * body *)
  | Break of pos_t
  | Let of pos_t * decl list * exp
  | Arr of pos_t * symbol * exp * exp
  (** type * size * init *)
and decl =
  | FunctionDecl of (pos_t * funcdecl) list
  (** a list for functions *)
  | VarDecl of pos_t * symbol * symbol option * exp
  (** name * type * init *)
  | TypeDecl of (pos_t * symbol * ty) list
  (** name * type *)
and op =
  | OpPlus | OpMinus | OpTimes | OpDiv | OpEq | OpNeq
  | OpLt | OpGt | OpLe | OpGe
and var =
  | VarId of pos_t * symbol (** symbol in lvalue *)
  | VarField of pos_t * var * symbol (** lvalue . symbol *)
  | VarSubscript of pos_t * var * exp (** lvalue [ exp ] *)
and ty =
  | NameTy of pos_t * symbol
  | RecordTy of fieldTy list
  | ArrayTy of pos_t * symbol
and fieldTy = {
  fldName: symbol;
  ty: symbol;
  pos: pos_t;
}
and funcdecl = {
  funName: symbol;
  fparams: fieldTy list;
  fresult: symbol option;
  fbody: exp;
}

let get_exp_pos = function
  | String (p, _) -> p
  | Int (p, _) -> p
  | Nil (p) -> p
  | Var (p, _) -> p
  | Op (p, _, _, _) -> p
  | Assign (p, _, _) -> p
  | Call (p, _, _) -> p
  | Record (p, _, _) -> p
  | Seq (p, _) -> p
  | If (p, _, _, _) -> p
  | While (p, _, _) -> p
  | For (p, _, _, _, _) -> p
  | Break (p) -> p
  | Let (p, _, _) -> p
  | Arr (p, _, _, _) -> p

let rec exp_to_string e =
  failwith "AST NYI"
(*
  let open Printf in
  let open PPrint in
  let pos_str = Pos.to_string (get_exp_pos e) in
  match e with
  | String (_, s) -> text s
  | Int (_, i) -> text (string_of_int i)
  | Nil (_) -> text "Nil"
  | Var (_, var) -> text (var_to_string var)
  | Op (_, op, e0, e1) ->
    text (op_to_string op) <->
    text (exp_to_string e0) <->
    text (exp_to_string e1)
  | Assign (_, var, e) ->
    text (var_to_string var) <-> text ":=" <-> (exp_to_string e)
  | Call (_, f, args) ->
    text (Symbol.to_string f) <->
    text "(" <->
    (List.fold_left (<->) Nil (List.map (fun e -> exp_to_tring e) args)) <->
    text ")"
  | Record(_, t, flds) ->
    sprintf "%s{%s}" (Symbol.to_string t)
      (String.concat ", "
         (List.map (fun (_, sym, e) ->
              sprintf "%s : %s" (Symbol.to_string sym) (exp_to_string e)
            ) flds))
  | Seq (_, es) ->
    String.concat "; " (List.map exp_to_string es)
  | If (_, tst, thn, els) ->
    let thnpart = sprintf
        "if (%s) then %s" (exp_to_string tst) (exp_to_string thn) in
    let elspart = match els with
      | None -> ""
      | Some (e) ->" else " ^ (exp_to_string e) in
    thnpart ^ elspart
  | While (_, tst, body) ->
    sprintf "while (%s) begin\n%s\nend"
      (exp_to_string tst) (exp_to_string body)
  | For (_, var, lo, hi, body) ->
    let vars = Symbol.to_string var in
    let los = exp_to_string lo in
    let his = exp_to_string hi in
    let bodys = exp_to_string body in
    sprintf "for (%s=%s; %s<%s; %s:=%s+1) begin\n%s\nend"
      vars los vars his vars vars bodys
  | Break _ -> "Break;\n"
  | _ -> failwith
*)
