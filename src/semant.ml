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

let rec transDecl (curr_level : Translate.level) (tenv : Types.typeEnv) (venv : Types.valEnv) (decls : S.decl list) : (Types.typeEnv * Types.valEnv) =
  let trfieldTy (te : Types.typeEnv) fld =
    match SymbolTable.look fld.S.ty te with
    | None -> raise_undef fld.S.pos fld.S.ty
    | Some (t) -> (fld.S.fldName, t)
  in
  (* Translate a list of TypeDecl. tenv already includes the 'header'
   * of each declaration. i.e. For any type declaration, tenv includes name -> NAME(None) *)
  let rec trtype_decl tenv (decl : (Pos.t * Symbol.t * S.ty) list) : Types.typeEnv = match decl with
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
  let rec trfunc_decl curr_level tenv venv (decl : (Pos.t * S.funcdecl) list) : (Types.typeEnv * Types.valEnv) = match decl with
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
       let body_ir, body_t = transExp level tenv venv' fbody in
       if ret_t <> body_t then
         expect_type pos (Types.t_to_string ret_t) body_t
       else
         trfunc_decl curr_level tenv venv tl
  in
  let rec check_multi_def (lst : (Pos.t * Symbol.t) list) : unit =
    match lst with
    | [] -> ()
    | (p, name) :: tl ->
       if List.exists (fun (newp, name2) -> name = name2) tl then
         raise (TypeError(p, "Multiple definition of " ^ (Symbol.to_string name)))
       else check_multi_def tl
  in
  match decls with
  | [] -> tenv, venv
  | hd :: tl ->
     let tenv', venv' =
       begin match hd with
       | S.VarDecl(pos, name, decl_ty, init) ->
          let init_ir, init_t = transExp curr_level tenv venv init in
          let acc = Translate.alloc_local curr_level true in
          let declared_t = match decl_ty with
            | None ->
               if init_t = Types.NIL then
                 raise (TypeError(pos, "You must declare the type of variable " ^ (Symbol.to_string name)))
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
          let venv' = SymbolTable.enter name (Types.VarType (acc, declared_t)) venv in
          tenv, venv'
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
          trtype_decl tenv' lst, venv
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
          trfunc_decl curr_level tenv venv' lst
       end in
     transDecl curr_level tenv' venv' tl

and transExp (curr_level : Translate.level) (tenv : Types.typeEnv) (venv : Types.valEnv) (expr : S.exp) : expty =
  let rec trvar (var : S.var) : expty =
    match var with
    | S.VarId (pos, sym) -> begin
        match SymbolTable.look sym venv with
        | None -> raise_undef pos sym
        | Some (Types.VarType(acc, t)) -> Translate.dummy_exp, t
        | Some (typ) -> expect_vtype pos "non-function" typ
      end
    | S.VarField (pos, var1, sym) -> begin
        match trvar var1 with
        | _, Types.RECORD (lst, _) ->
           begin try List.assoc sym lst
                 with
                   Not_found -> raise_undef pos sym
           end
        | _, t -> expect_type pos "record" t

      end
    | S.VarSubscript(pos, var1, e) ->
       match trvar var1 with
       | Types.ARRAY (t, _) ->
          begin match trexp e with
          | Types.INT -> t
          | t' -> expect_type pos "int" t'
          end
       | t' -> expect_type pos "array" t'
  and trexp (exp : S.exp) : expty =
    match exp with
    | S.Int (_, _) -> Types.INT
    | S.Var (_, var) -> trvar var
    | S.String (_) -> Types.STRING
    | S.Nil (_) -> Types.NIL
    | S.Break (_) -> Types.UNIT
    | S.Op (pos, op, l, r) ->
       let left_ty = trexp l in
       let right_ty = trexp r in
       begin match op with
       | S.OpPlus | S.OpMinus | S.OpTimes | S.OpDiv | S.OpLt | S.OpGt | S.OpLe | S.OpGe ->
          if left_ty == Types.INT && right_ty = Types.INT then
            Types.INT
          else
            raise (TypeError(pos, "Operator applied to non-integral types: " ^
                                    (Types.t_to_string left_ty) ^ " and " ^
                                      (Types.t_to_string right_ty)))
       | S.OpEq | S.OpNeq ->
          if left_ty <> right_ty then
            raise (TypeError(pos, "Operator applied to different types: " ^
                                    (Types.t_to_string left_ty) ^ " and " ^
                                      (Types.t_to_string right_ty)))
          else left_ty
       end
    | S.Assign (pos, var, e) ->
       let left_ty = trvar var in
       let right_ty = trexp e in
       if left_ty <> right_ty then
         expect_type pos (Types.t_to_string left_ty) right_ty
       else Types.UNIT
    | S.Call (pos, f, args) -> begin
        match SymbolTable.look f venv with
        | Some (Types.VarType(_)) ->
           raise (TypeError(pos, (Symbol.to_string f) ^ " is not applicable"))
        | Some (Types.FuncType (level, arg_t, ret_t)) ->
           let rec check_arg (expect : Types.t list) (actual : S.exp list) : unit = match expect, actual with
             | [], [] -> ()
             | _, [] | [], _ -> raise (TypeError(pos, sprintf "Arity mismatch. Expected %d but got %d"
                                                             (List.length arg_t) (List.length args)))
             | hd :: tl, hd' :: tl' ->
                let actual_t = trexp hd' in
                if actual_t <> hd then expect_type (S.get_exp_pos hd') (Types.t_to_string hd) actual_t
                else check_arg tl tl'
           in
           check_arg arg_t args;
           ret_t
        | None -> raise_undef pos f
      end
    | S.Record (pos, record, fields) ->
       begin match SymbolTable.look record tenv with
       | None -> raise_undef pos record
       | Some (record) -> begin
           (match record with
            | Types.RECORD(lst, uniq) ->
               List.iter (fun (pos, sym, e) ->
                   (* first see if sym is in the record type. *)
                   match Types.record_find lst sym with
                   | None -> raise_undef pos sym
                   | Some (t) ->
                      (* 2. see if e's type matches declared type *)
                      let e_t = trexp e in
                      if t = e_t || e_t = Types.NIL then ()
                      else expect_type pos (Types.t_to_string t) e_t
                 ) fields
            | _ -> expect_type pos "record" record);
           record
         end
       end
    | S.Seq (_, lst) ->
       let rec trexp_list = function
         | [] -> Types.UNIT
         | hd :: [] -> trexp hd
         | hd :: tl -> ignore(trexp hd); trexp_list tl
       in
       trexp_list lst
    | S.If (pos, tst, thn, None) ->
       let tst_t = trexp tst in
       if tst_t <> Types.INT then
         expect_type (S.get_exp_pos tst) "int" tst_t
       else
         begin match trexp thn with
         | Types.UNIT -> Types.UNIT
         | thn_t -> expect_type (S.get_exp_pos thn) "unit" thn_t
         end
    | S.If (pos, tst, thn, Some (els)) ->
       let tst_t = trexp tst in
       if tst_t <> Types.INT then
         expect_type (S.get_exp_pos tst) "int" tst_t
       else
         let thn_t = trexp thn in
         let els_t = trexp els in
         if thn_t = els_t then thn_t
         else expect_type (S.get_exp_pos els) (Types.t_to_string thn_t) els_t
    | S.While (pos, tst, body) ->
       let tst_t = trexp tst in
       if tst_t <> Types.INT then
         expect_type (S.get_exp_pos tst) "int" tst_t
       else let body_t = trexp body in
            if body_t <> Types.UNIT then
              expect_type (S.get_exp_pos body) "unit" body_t
            else Types.UNIT
    | S.For (pos, v, lo, hi, body) ->
       (** For exp implicitly binds v to the type of lo/hi in the body *)
       begin match trexp lo, trexp hi with
       | Types.INT, Types.INT ->
          let acc = Translate.alloc_local curr_level true in
          let venv' = SymbolTable.enter v (Types.VarType(acc, Types.INT)) venv in
          let body_t = transExp curr_level tenv venv' body in
          if body_t <> Types.UNIT then
            expect_type (S.get_exp_pos body) "unit" body_t
          else body_t
       | lo_t, Types.INT ->
          expect_type (S.get_exp_pos lo) "int" lo_t
       | Types.INT, hi_t ->
          expect_type (S.get_exp_pos hi) "int" hi_t
       | lo_t, _ ->
          expect_type (S.get_exp_pos lo) "int" lo_t
       end
    | S.Let (pos, decl, body) ->
       let tenv', venv' = transDecl curr_level tenv venv decl in
       transExp curr_level tenv' venv' body
    | S.Arr (pos, typ, size, init) ->
       begin match SymbolTable.look typ tenv with
       | Some(Types.ARRAY(t, uniq)) ->
          let size_t = trexp size in
          if size_t <> Types.INT then
            expect_type (S.get_exp_pos size) "int" size_t
          else
            let init_t = trexp init in
            if init_t <> t then
              expect_type (S.get_exp_pos init) (Types.t_to_string t) init_t
            else
              Types.ARRAY(t, uniq)
       | Some(other_t) ->
          expect_type pos "array" other_t
       | None ->
          raise_undef pos typ
       end
  in
  trexp expr

let transProg (e : S.exp) : unit =
  ignore(transExp Translate.outermost Types.typeEnv Types.valEnv e)
