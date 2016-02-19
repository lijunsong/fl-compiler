open Syntax
open Batteries

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
  | Seq(_, lst), Seq(_, lst') ->
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
    | VarId(_, id), VarId(_, id') -> id = id'
    | VarField (_, v, id), VarField(_, v', id') ->
       id = id' && ast_var_equal v v'
    | VarSubscript (_, v, e), VarSubscript(_, v', e') ->
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

let ir_eq (l1 : Ir.stmt list) (l2 : Ir.stmt list) : bool =
  (* Algorithm to compare two ir lists which contains differnt label
     and temp at the same location:

     1. when comparing two labels l1 and l2, if it hasn't seen l1 and
     l2 before, create maps from l1 to l2 and l2 to l1
     2. If it has seen either l1 or l2, the two ir's does not match
     3. (else it has seen both), get what l1 maps to, compare with l2
     to get the result.

     temp is the same.
  *)
  let tmap = Hashtbl.create 32 in
  let lmap = Hashtbl.create 32 in
  let lmem = Hashtbl.mem lmap in
  let tmem = Hashtbl.mem tmap in
  let ladd = hashtbl.add lmap in
  let tadd = Hashtbl.add tmap in
  let lget = Hashtbl.find lmap in
  let tget = Hashtbl.find tmap in
  let cmp_label l1 l2 =
    (* In our impl, no two labels in different ir shall be the same *)
    assert (l1 <> l2)
    if lmem l1 <> lmem l2 then false
    else if lmem l1 then lget l1 = l2
    else (ladd l1 l2; ladd l2 l1; true)
  in
  let cmp_temp t1 t2 =
    assert (t1 <> t2)
    if tmem t1 <> tmem t2 then false
    else if tmem t1 then tget t1 = t2
    else (tadd t1 t2; tadd t2 t1; true)
  in
  let rec eq_stmt ir1 ir2 =
    match ir1, ir2 with
    | Ir.LABEL(l1), Ir.LABEL(l2) -> cmp_label l1 l2
    | Ir.MOVE(e1, e2), Ir.MOVE(e3, e4) ->
      eq_exp e1 e3 && eq_exp e2 e4
    | Ir.EXP(e1), Ir.EXP(e2) -> eq_exp e1 e2
    | Ir.JUMP (e1, l), Ir.JUMP(e2, l') ->
      eq_exp e1 e2 && List.all (List.map2 cmp_label l l')
    | Ir.CJUMP (r1, e1, e2, l1, l2),
      Ir.CJUMP (r1', e1', e2', l1', l2') ->
      r1 = r1' && eq_exp e1 e1' && eq_exp e2 e2' &&
      cmp_label l1 l1' && cmp_label l2 l2'
    | SEQ(s1, s2), SEQ(s1', s2') ->
      eq_stmt s1 s1' && eq_stmt s2 s2'
    | _ -> false
  and eq_exp e1 e2 = match e1, e2 with
    | Ir.CONST (c), Ir.CONST(c') -> c = c'
    | Ir.NAME(l), Ir.NAME(l') -> cmp_label l l'
    | Ir.TEMP(t), Ir.TEMP(t') -> cmp_temp t t'
    | Ir.BINOP(op, e1, e2), Ir.BINOP(op', e1', e2') ->
      op = op' && eq_exp e1 e1' && eq_exp e2 e2'
    | Ir.MEM(e), Ir.MEM(e') -> eq_exp e e'
    | Ir.CALL(e, es), Ir.CALL(e', es') ->
      eq_exp e e' && List.all (List.map2 eq_exp es es')
    | Ir.ESEQ(s, e), Ir.ESEQ(s', e') ->
      eq_stmt s s' && eq_exp e e'
  in
  List.all (List.map2 eq_stmt l1 l2)
