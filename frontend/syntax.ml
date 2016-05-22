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

let text_enclose name doc =
  let open Pprint in
  let s = name ^ "(" in
  text s <-> nest (String.length s) doc <-> text ")"

let text_comma = Pprint.text ","
let text_comma_line =
  let open Pprint in
  text_comma <-> line

let symbol_to_text s =
  Pprint.text (Symbol.to_string s)

let rec exp_to_doc e =
  let open Printf in
  let open Pprint in
  (*let pos_str = Pos.to_string (get_exp_pos e) in*)
  match e with
  | String (_, s) -> text ("\"" ^ s ^ "\"")
  | Int (_, i) -> text (string_of_int i)
  | Nil (_) -> text "Nil"
  | Var (_, var) -> var_to_doc var
  | Op (_, op, e0, e1) ->
    text_enclose "Op"
      (concat text_comma_line
         [op_to_doc op;
          exp_to_doc e0;
          exp_to_doc e1])
  | Assign (_, var, e) ->
    text_enclose "Assign" (var_to_doc var
                           <-> text " := "
                           <-> exp_to_doc e)
  | Call (_, f, args) ->
    text_enclose "Call" (concat text_comma_line
                           (text (Symbol.to_string f)
                            :: List.map exp_to_doc args))
  | Record(_, t, flds) ->
    text_enclose "Record" (
      text (Symbol.to_string t) <-> text ";"
      <-> line
      <-> text "["
      <-> nest 1 (concat text_comma_line
                    (List.map (fun (_, sym, e) ->
                         text (Symbol.to_string sym)
                         <-> text ": " <-> exp_to_doc e)
                        flds))
    )
  | Seq (_, es) ->
    text_enclose "Seq" (concat text_comma_line (List.map exp_to_doc es))
  | If (_, tst, thn, els) ->
    let thnpart = exp_to_doc tst <-> text_comma_line
                  <-> exp_to_doc thn in
    let elspart = match els with
      | None -> Nil
      | Some (e) -> line <-> exp_to_doc e in
    text_enclose "If" (thnpart <-> elspart)
  | While (_, tst, body) ->
    text_enclose "While"
      (exp_to_doc tst <-> text_comma_line
       <-> exp_to_doc body)
  | For (_, var, lo, hi, body) ->
    let vars = text (Symbol.to_string var) in
    let los = exp_to_doc lo in
    let his = exp_to_doc hi in
    let body = exp_to_doc body in
    text_enclose "For" (concat line [vars; los; his; body])
  | Break _ -> text "Break"
  | Let (_, decls, body) ->
    let decls' = concat line (List.map decl_to_doc decls) in
    text_enclose "Let" (decls' <-> line <-> exp_to_doc body)
  | Arr (_, t, sz, init) ->
    text_enclose "Arr"
      (symbol_to_text t <-> text_comma_line
       <-> exp_to_doc sz <-> text_comma_line
       <-> exp_to_doc init)
and decl_to_doc d =
  let open Pprint in
  match d with
  | FunctionDecl (decl_list) ->
    concat line (List.map (fun (_, decl) ->
        let args = concat text_comma (List.map (fun ft ->
            fieldTy_to_doc ft) decl.fparams)
                   |> text_enclose "args" in
        text_enclose "FunctionDecl"
          (symbol_to_text decl.funName <-> text_comma_line
           <-> args <-> text_comma_line
           <-> exp_to_doc decl.fbody)) decl_list)
  | VarDecl (_, name, t, init) ->
    text_enclose "VarDecl"
      (symbol_to_text name <-> text_comma_line
       <-> exp_to_doc init)
  | TypeDecl decls ->
    concat line (List.map (fun (_, name, t) ->
        text_enclose "TypeDecl"
          (symbol_to_text name <-> text_comma_line
           <-> ty_to_doc t))
        decls)
and var_to_doc v =
  let open Pprint in
  match v with
  | VarId (_, s) ->
    text_enclose "VarId" (symbol_to_text s)
  | VarField (_, v, s) ->
    text_enclose "VarField" (var_to_doc v <-> text_comma <-> symbol_to_text s)
  | VarSubscript (_, v, e) ->
    text_enclose "VarSubscript" (var_to_doc v <-> text_comma <-> exp_to_doc e)

and op_to_doc o =
  let open Pprint in
  match o with
  | OpPlus -> text "+"
  | OpMinus -> text "-"
  | OpTimes -> text "x"
  | OpDiv -> text "/"
  | OpEq -> text "=="
  | OpNeq -> text "!="
  | OpLt -> text "<"
  | OpGt -> text ">"
  | OpLe -> text "<="
  | OpGe -> text ">="
and ty_to_doc t =
  let open Pprint in
  match t with
  | NameTy (_, s) -> symbol_to_text s
  | RecordTy (fd) ->
    concat text_comma_line (List.map (fun ft -> fieldTy_to_doc ft) fd)
  | ArrayTy (_, s) -> symbol_to_text s
and fieldTy_to_doc ft =
  let open Pprint in
  text_enclose "fieldTy"
    (symbol_to_text ft.fldName <-> text_comma_line
     <-> symbol_to_text ft.ty)

let exp_to_string e =
  exp_to_doc e |> Pprint.layout
