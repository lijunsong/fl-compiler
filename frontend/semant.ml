module S = Syntax
open Printf
open Symbol
open Batteries

(** Type Chcker

Notation:
 - "te": type environment
 - "te |- E : T": expression E in type envionrment te has type T
 - "te(I)" : designates the type assigned to symbol I in te
               premise1, premise2, etc.
 - type rule  -------------------------
                   conclusion


 1. te |- Int : INT
 2. te |- String : STRING
 3. te |- Nil : NIL
 4. te |- Var(VarId(s)) : te(s)

 5. te |- var : RECORD([fld: T]),
    te |- fld : STRING
    ------------------------------------------------
    te |- Var(VarField(var, fld)) : T

 6. te |- var : ARRAY of T,
    te |- E : INT
    ----------------------
    te |- Var(VarSubscript(var, E))
 7. ...

 **)

type expty = Translate.exp * Types.t

exception InternalError of string
(** [msg] InternalError *)

exception TypeError of Pos.t * string
(** [pos * message]: TypeError *)

exception UndefinedError of Pos.t * string
(** [pos * message]: undefined variable or field *)

let raise_undef pos (sym : Symbol.t) =
  raise (UndefinedError(pos, "Undefined " ^ (Symbol.to_string sym)))

let expect_type pos (expect : string) (actual : Types.t) =
  raise (TypeError(pos, "Expected type " ^ expect ^ " but got " ^ (Types.t_to_string actual)))

let expect_vtype pos (expect : string) (actual : Types.typ) =
  raise (TypeError(pos, "Expected type " ^ expect ^ " but got " ^ (Types.typ_to_string actual)))

(** given a type-id (symbol), get the actual Type or issue an error *)
let get_type pos (sym : Symbol.t) (tenv : Types.typeEnv) : Types.t =
  match SymbolTable.look sym tenv with
  | None -> raise_undef pos sym
  | Some (t) -> t

let trans_binop (op : S.op) : Ir.binop = match op with
  | S.OpPlus -> Ir.PLUS
  | S.OpMinus -> Ir.MINUS
  | S.OpTimes -> Ir.MUL
  | S.OpDiv -> Ir.DIV
  | _ -> failwith "Unknown Binary Operator"

let trans_relop (op : S.op) : Ir.relop = match op with
  | S.OpEq -> Ir.EQ
  | S.OpNeq -> Ir.NE
  | S.OpLt -> Ir.LT
  | S.OpGt -> Ir.GT
  | S.OpLe -> Ir.LE
  | S.OpGe -> Ir.GE
  | _ -> failwith "Unknown Relational Operator "

(** Desugar
 *     for i := lo to hi do body
 *  to
 *     let var i := lo
 *         var %limit := hi in
 *     if (i <= %limit)
 *        while 1 do (
 *          body
 *          if (i < %limit) then i = i + 1  // extra_if here
 *          else break
 *        )
 *)
let desugar_forloop (e : S.exp) : S.exp =
  let dummy = Pos.dummy in
  match e with
  | S.For (pos, var, lo, hi, body) ->
     let limit = Symbol.of_string "%limit" in
     let id_var = S.Var(pos, S.VarId(dummy, var)) in
     let id_limit = S.Var (dummy, S.VarId(dummy, limit)) in
     let extra_if = S.If(pos, S.Op(pos, S.OpLt, id_var, id_limit),
                         S.Assign(dummy, S.VarId(dummy, var),
                                  S.Op(dummy, S.OpPlus, id_var, S.Int(dummy, 1))),
                         Some (S.Break(dummy))) in
     let new_body = S.Seq(dummy, [body; extra_if]) in
     S.Let (dummy,
            [S.VarDecl(pos, var, None, lo);
             S.VarDecl(dummy, limit, None, hi)],
            S.If(dummy, S.Op(pos, S.OpLe, id_var, id_limit),
                 S.While(pos, S.Int(dummy, 1), new_body),
                 None))
  | _ -> failwith "unreachable in desugar_forloop"


(** [trans_decl curr_level typeenv valenv decls] trans_decl translates
 * Let bindings. It returnes augmented type env, value env and a list
 * of initializations. *)
let rec trans_decl (curr_level : Translate.level) (tenv : Types.typeEnv)
                   (venv : Types.valEnv) (decls : S.decl list)
        : Types.typeEnv * Types.valEnv * Translate.exp list =
  let trfieldTy (te : Types.typeEnv) fld =
    match SymbolTable.look fld.S.ty te with
    | None -> raise_undef fld.S.pos fld.S.ty
    | Some (t) -> (fld.S.fldName, t)
  in
  (* Translate a list of TypeDecl. tenv already includes the 'header'
   * of each declaration. i.e. For any type declaration, tenv includes
   * name -> NAME(None) *)
  let rec trtype_decl tenv (decl : (Pos.t * Symbol.t * S.ty) list)
          : Types.typeEnv = match decl with
    | [] -> tenv
    | (pos, name, ty) :: tl ->
       let t' =
         begin match ty with
         | S.NameTy (pos, typeid) ->
            begin match SymbolTable.look typeid tenv with
            | Some (t) -> t
            | None -> raise_undef pos typeid
            end
         | S.RecordTy (fld_list) ->
            let rec_fields = List.map (fun fld -> trfieldTy tenv fld) fld_list in
            let t = Types.RECORD (rec_fields, Types.Uniq.uniq()) in
            t
         | S.ArrayTy (pos, sym) ->
            begin match SymbolTable.look sym tenv with
            | Some (t) -> Types.ARRAY(t, Types.Uniq.uniq())
            | None -> raise_undef pos sym
            end
         end
       in
       trtype_decl (SymbolTable.enter name t' tenv) tl
  in
  let rec trfunc_decl curr_level tenv venv (decl : (Pos.t * S.funcdecl) list)
          : (Types.typeEnv * Types.valEnv) = match decl with
    | [] -> tenv, venv
    | (pos, {S.funName;S.fparams;S.fresult;S.fbody}) :: tl ->
       (** Functions must have been in the env. So, for each function,
       fetch its level information, add its formals to a new env and
       continue translate body *)
       let level, args_t, ret_t = match SymbolTable.look funName venv with
         | None -> raise (InternalError("Function is not preproecssed."))
         | Some(Types.VarType(_)) -> raise (InternalError((Symbol.to_string funName) ^ " is not a function."))
         | Some(Types.FuncType(lev, arg, ret)) -> lev, arg, ret
       in
       let args_access : (Translate.access * Types.t) list =
         List.combine (Translate.get_formals level) args_t in
       let args_sym : Symbol.t list = List.map (fun p -> p.S.fldName) fparams in
       let venv' = List.fold_right2 (fun name (acc, t) table -> (* binds args *)
                       SymbolTable.enter name (Types.VarType(acc, t)) table) args_sym args_access venv in
       let body_ir, body_t = trans_exp level tenv venv' fbody in
       if ret_t <> body_t then
         expect_type pos (Types.t_to_string ret_t) body_t
       else begin
         Translate.proc_entry_exit curr_level body_ir;
         trfunc_decl curr_level tenv venv tl
       end
  in
  let rec check_multi_def (lst : (Pos.t * Symbol.t) list) : unit =
    match lst with
    | [] -> ()
    | (p, name) :: tl ->
       if List.exists (fun (newp, name2) -> name = name2) tl then
         raise (TypeError(p, "Multiple definition of " ^ (Symbol.to_string name)))
       else check_multi_def tl
  in
  let rec trans_iter decls tenv venv inits =
    match decls with
    | [] -> tenv, venv, inits
    | hd :: tl ->
       begin match hd with
         | S.VarDecl(pos, name, decl_ty, init) ->
            let init_ir, init_t = trans_exp curr_level tenv venv init in
            let acc = Translate.alloc_local curr_level true in
            let declared_t = match decl_ty with
              | None ->
                 if init_t = Types.NIL then
                   raise (TypeError(pos, "You must declare the type of variable "
                                         ^ (Symbol.to_string name)))
                 else init_t
              | Some (decl_t) ->
                 begin match get_type pos decl_t tenv with
                 | Types.RECORD (_) as rec_type ->
                    if init_t <> rec_type && init_t <> Types.NIL then
                      expect_type pos (Symbol.to_string decl_t) init_t
                    else init_t
                 | t -> if init_t <> t then
                         expect_type pos (Symbol.to_string decl_t) init_t
                       else init_t
                 end
            in
            let venv' = SymbolTable.enter
                          name (Types.VarType (acc, declared_t)) venv in
            let init' = Translate.assign (Translate.simple_var acc curr_level)
                                         init_ir :: inits in
            trans_iter tl tenv venv' init'
         | S.TypeDecl (lst) ->
            check_multi_def (List.map (fun (p, sym, _) -> p, sym) lst);
            let valid_recursive = List.filter (fun (_,_,ty) ->
                                      match ty with
                                      | S.RecordTy (_) -> true
                                      | S.ArrayTy (_) -> true
                                      | S.NameTy (_) -> false) lst in
            let name_t = List.map (fun (pos, s, _) ->pos, s, Types.NAME(s, ref None)) valid_recursive in
            let tenv' = List.fold_right (fun (pos, name,t) table ->
                            SymbolTable.enter name t table) name_t tenv in
            let tenv'' = trtype_decl tenv' lst in
            trans_iter tl tenv'' venv inits
         | S.FunctionDecl (lst) ->
            check_multi_def (List.map (fun (p, f) -> p, f.S.funName) lst);
            (** Construct a FuncType for each f in lst before checking functions *)
            let func_list : (Symbol.t * Translate.level * Types.t list * Types.t) list =
              List.map (fun (pos,func) ->
                  let label = Temp.new_label ~prefix:(Symbol.to_string func.S.funName) () in
                  let level = Translate.new_level curr_level label (List.map (fun _ -> true) func.S.fparams) in
                  let params_t = List.map (fun p -> let _, t = trfieldTy tenv p in t) func.S.fparams in
                  let ret_t = match func.S.fresult with
                    | None -> Types.UNIT
                    | Some (t) -> get_type pos t tenv in
                  func.S.funName, level, params_t, ret_t) lst in
            let venv' = List.fold_right (fun (name, level, arg, ret) table ->
                            SymbolTable.enter name (Types.FuncType(level, arg, ret)) table) func_list venv in
            let tenv', venv'' = trfunc_decl curr_level tenv venv' lst in
            trans_iter tl tenv' venv'' inits
       end
  in
  trans_iter decls tenv venv []

  and trans_exp (curr_level : Translate.level) (tenv : Types.typeEnv) (venv : Types.valEnv) (expr : S.exp) : expty =
    let rec trvar (var : S.var) : expty =
      match var with
      | S.VarId (pos, sym) -> begin
          match SymbolTable.look sym venv with
          | None -> raise_undef pos sym
          | Some (Types.VarType(acc, t)) ->
             Translate.simple_var acc curr_level, t
          | Some (typ) -> expect_vtype pos "non-function" typ
        end
      | S.VarField (pos, var1, sym) -> begin
          match trvar var1 with
          | base, Types.RECORD (lst, _) ->
             begin
               match Translate.var_field base sym lst with
               | None -> raise_undef pos sym
               | Some (e) -> e
             end
          | _, t -> expect_type pos "record" t

        end
      | S.VarSubscript(pos, var1, e) ->
         match trvar var1 with
         | var_ir, Types.ARRAY (t, _) ->
            begin match trexp e with
            | e_ir, Types.INT -> Translate.var_subscript var_ir e_ir, t
            | _, t' -> expect_type pos "int" t'
            end
         | _, t' -> expect_type pos "array" t'
    and trexp (exp : S.exp) : expty =
      match exp with
      | S.Int (_, i) -> Translate.const i, Types.INT
      | S.Var (_, var) -> trvar var
      | S.String (_, s) -> Translate.string s, Types.STRING
      | S.Nil (_) ->
         (* Semantics: the value of a record *)
         Translate.nil (), Types.NIL
      | S.Break (_) -> Translate.break() , Types.UNIT
      | S.Op (pos, op, l, r) ->
         let l_ir, left_ty = trexp l in
         let r_ir, right_ty = trexp r in
         begin match op with
         | S.OpPlus | S.OpMinus | S.OpTimes | S.OpDiv ->
            let binop = trans_binop op in
            if left_ty == Types.INT && right_ty = Types.INT then
              Translate.binop binop l_ir r_ir, Types.INT
            else
              raise (TypeError(pos, "Operator applied to non-integral types: " ^
                                      (Types.t_to_string left_ty) ^ " and " ^
                                        (Types.t_to_string right_ty)))
         | S.OpLt | S.OpGt | S.OpLe | S.OpGe ->
            let relop = trans_relop op in
            if left_ty == Types.INT && right_ty = Types.INT then
              Translate.relop relop l_ir r_ir, Types.INT
            else
              raise (TypeError(pos, "Operator applied to non-integral types: " ^
                                      (Types.t_to_string left_ty) ^ " and " ^
                                        (Types.t_to_string right_ty)))
         | S.OpEq | S.OpNeq ->
            let relop = trans_relop op in
            if left_ty = right_ty then
              Translate.relop relop l_ir r_ir, left_ty
            else
              raise (TypeError(pos, "Operator applied to different types: " ^
                                      (Types.t_to_string left_ty) ^ " and " ^
                                        (Types.t_to_string right_ty)))
         end
      | S.Assign (pos, var, e) ->
         let lhs_ir, left_ty = trvar var in
         let rhs_ir, right_ty = trexp e in
         if left_ty <> right_ty then
           expect_type pos (Types.t_to_string left_ty) right_ty
         else
           (* Ah! the overloaded MEM actually simplies the translation
         of assign. *)
           Translate.assign lhs_ir rhs_ir, Types.UNIT
      | S.Call (pos, f, args) -> begin
          match SymbolTable.look f venv with
          | None -> raise_undef pos f
          | Some (Types.VarType(_)) ->
             raise (TypeError(pos, (Symbol.to_string f) ^ " is not applicable"))
          | Some (Types.FuncType (level, arg_t, ret_t)) ->
             let rec check_arg (expect : Types.t list) (actual : S.exp list) : Translate.exp list = match expect, actual with
               | [], [] -> []
               | _, [] | [], _ -> raise (TypeError(pos, sprintf "Arity mismatch. Expected %d but got %d"
                                                               (List.length arg_t) (List.length args)))
               | hd :: tl, hd' :: tl' ->
                  let actual_ir, actual_t = trexp hd' in
                  if actual_t <> hd then
                    expect_type (S.get_exp_pos hd') (Types.t_to_string hd) actual_t
                  else
                    actual_ir :: check_arg tl tl'
             in
             let argsv = check_arg arg_t args in
             Translate.call level argsv, ret_t
        end
      | S.Record (pos, record, fields) ->
         begin match SymbolTable.look record tenv with
         | None -> raise_undef pos record
         | Some (record) -> begin
             match record with
             | Types.RECORD(lst, uniq) ->
                let flds = List.map2 (fun (pos, s0, e0) (s1, expect_t) ->
                    (* s0, e0 is the constructor, s1, expect_t is what
                       user declared, see if they match. *)
                    if s0 <> s1 then
                      raise_undef pos s0
                    else let e_ir, e_t = trexp e0 in (* check type *)
                      if expect_t = e_t || e_t = Types.NIL then
                        e_ir
                      else
                        expect_type pos (Types.t_to_string expect_t) e_t
                  ) fields lst in
                Translate.record flds, record
             | _ -> expect_type pos "record" record
           end
         end
      | S.Seq (_, lst) -> begin match lst with
          | [] -> Translate.no_value(), Types.UNIT
          | lst -> Translate.seq (List.map trexp lst)
        end
      | S.If (pos, tst, thn, None) ->
         let tst_ir, tst_t = trexp tst in
         if tst_t <> Types.INT then
           expect_type (S.get_exp_pos tst) "int" tst_t
         else
           begin match trexp thn with
           | thn_ir, Types.UNIT ->
              Translate.if_cond_unit_body tst_ir thn_ir None, Types.UNIT
           | _, thn_t -> expect_type (S.get_exp_pos thn) "unit" thn_t
           end
      | S.If (pos, tst, thn, Some (els)) ->
         let tst_ir, tst_t = trexp tst in
         if tst_t <> Types.INT then
           expect_type (S.get_exp_pos tst) "int" tst_t
         else
           let thn_ir, thn_t = trexp thn in
           let els_ir, els_t = trexp els in
           if thn_t <> els_t then
             expect_type (S.get_exp_pos els) (Types.t_to_string thn_t) els_t
           else if thn_t = Types.UNIT then
             Translate.if_cond_unit_body tst_ir thn_ir (Some els_ir), Types.UNIT
           else
             Translate.if_cond_nonunit_body tst_ir thn_ir (Some els_ir), thn_t
      | S.While (pos, tst, body) ->
         let tst_ir, tst_t = trexp tst in
         if tst_t <> Types.INT then
           expect_type (S.get_exp_pos tst) "int" tst_t
         else let body_ir, body_t = trexp body in
              if body_t <> Types.UNIT then
                expect_type (S.get_exp_pos body) "unit" body_t
              else Translate.while_loop tst_ir body_ir, Types.UNIT
      | S.For (pos, v, lo, hi, body) ->
         (** For exp implicitly binds v to the type of lo/hi in the body *)
         begin match trexp lo, trexp hi with
         | (lo_ir, Types.INT), (hi_ir, Types.INT) ->
            let acc = Translate.alloc_local curr_level true in
            let venv' = SymbolTable.enter v (Types.VarType(acc, Types.INT)) venv in
            let _, body_t = trans_exp curr_level tenv venv' body in
            (* discard the translated body *)
            if body_t <> Types.UNIT then
              expect_type (S.get_exp_pos body) "unit" body_t
            else
              let new_forloop = desugar_forloop exp in
              trans_exp curr_level tenv venv new_forloop
         | (_, lo_t), (_, Types.INT) ->
            expect_type (S.get_exp_pos lo) "int" lo_t
         | (_, Types.INT), (_, hi_t) ->
            expect_type (S.get_exp_pos hi) "int" hi_t
         | (_, lo_t), _ ->
            expect_type (S.get_exp_pos lo) "int" lo_t
         end
      | S.Let (pos, decl, body) ->
         let tenv', venv', inits = trans_decl curr_level tenv venv decl in
         let body_ir, t = trans_exp curr_level tenv' venv' body in
         Translate.prepend_stmts inits body_ir, t

      | S.Arr (pos, typ, size, init) ->
         begin match SymbolTable.look typ tenv with
         | Some(Types.ARRAY(t, uniq)) ->
            let size_ir, size_t = trexp size in
            if size_t <> Types.INT then
              expect_type (S.get_exp_pos size) "int" size_t
            else
              let init_ir, init_t = trexp init in
              if init_t <> t then
                expect_type (S.get_exp_pos init) (Types.t_to_string t) init_t
              else
                Translate.array size_ir init_ir, Types.ARRAY(t, uniq)
         | Some(other_t) ->
            expect_type pos "array" other_t
         | None ->
            raise_undef pos typ
         end
    in
    trexp expr

let trans_prog (e : S.exp) : Translate.frag list =
  let body, t = trans_exp Translate.outermost Types.typeEnv Types.valEnv e in
  Translate.proc_entry_exit Translate.outermost body;
  Translate.get_result()


let type_check (e : S.exp) : unit =
  try
    ignore(trans_prog e)
  with
  | TypeError (pos, msg) ->
    printf "TypeError:%s: %s\n" (Pos.to_string pos) msg
  | UndefinedError (pos, msg) ->
    printf "TypeError:%s: %s\n" (Pos.to_string pos) msg
