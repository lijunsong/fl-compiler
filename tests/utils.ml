open Syntax

let cmp_list2 f l1 l2 =
  List.length l1 = List.length l2 &&
    List.for_all2 f l1 l2

let rec ast_equal ast1 ast2 =
  match ast1, ast2 with
  | String (_, a), String (_, b) -> a = b
  | Int (_, a), Int(_, b) -> a = b
  | Nil (_), Nil (_) -> true
  | Var (_, a), Var(_, b) -> ast_var_equal a b
  | Op (_, o, l, r), Op(_, o', l', r') ->
     o = o' && ast_equal l l' && ast_equal r r'
  | Assign (_, v, e), Assign(_, v', e') ->
     ast_var_equal v v' && ast_equal e e'
  | Call (_, s, lst), Call(_, s', lst') ->
     s = s' && cmp_list2 ast_equal lst lst'
  | Record(_, s, flds), Record (_, s', flds') ->
     s = s' && cmp_list2 (fun (_,s,e) (_,s',e') ->
                  s = s' && ast_equal e e') flds flds'
  | Seq(lst), Seq(lst') ->
     cmp_list2 ast_equal lst lst'
  | If (_, tst, thn, els), If (_, tst', thn', els') ->
     ast_equal tst tst' && ast_equal thn thn' &&
       (match els, els' with
        | None, None -> true
        | Some e, Some e' -> ast_equal e e'
        | _ -> false)
  | While (_, tst, body), While (_, tst', body') ->
     ast_equal tst tst' && ast_equal body body'
  | For (_, var, lo, hi, body), For (_, var', lo', hi', body') ->
     var = var' && ast_equal lo lo' &&
       ast_equal hi hi' && ast_equal body body'
  | Break (_), Break (_) -> true
  | Let (_, decls, body), Let (_, decls', body') ->
       cmp_list2 ast_decl_equal decls decls' &&
         ast_equal body body'
  | Arr (_, ty, size, init), Arr (_, ty', size', init') ->
     ty = ty' && ast_equal size size' && ast_equal init init'
  | _ -> false

and ast_var_equal var1 var2 = match var1, var2 with
    | VarId(id), VarId(id') -> id = id'
    | VarField (v, id), VarField(v', id') ->
       id = id' && ast_var_equal v v'
    | VarSubscript (v, e), VarSubscript(v', e') ->
       ast_var_equal v v' && ast_equal e e'
    | _ -> false

and ast_decl_equal decl1 decl2 =
  let fldTy_cmp f1 f2 =
    f1.fldName = f2.fldName && f1.ty = f2.ty
  in
  let funcdecl_cmp (_, d1) (_, d2) =
    d1.funName = d2.funName &&
      d1.fresult = d2.fresult &&
        ast_equal d1.fbody d2.fbody &&
          cmp_list2 fldTy_cmp d1.fparams d2.fparams
  in
  let ty_cmp t1 t2 = match t1, t2 with
    | NameTy(_, s), NameTy(_, s') -> s = s'
    | RecordTy(lst), RecordTy(lst') ->
       cmp_list2 fldTy_cmp lst lst'
    | ArrayTy(_, s), ArrayTy(_, s') -> s = s'
    | _ -> false
  in
  match decl1, decl2 with
  | FunctionDecl (lst), FunctionDecl(lst') ->
     cmp_list2 funcdecl_cmp lst lst'
  | VarDecl (_, s1, s2, e), VarDecl (_, s1', s2', e') ->
     s1 = s1' && s2 = s2' && ast_equal e e'
  | TypeDecl (lst), TypeDecl (lst') ->
     cmp_list2 (fun (_, s, ty) (_, s', ty') ->
         s = s' && ty_cmp ty ty') lst lst'
  | _ -> false
