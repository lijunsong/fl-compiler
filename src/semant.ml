module S = Syntax
open Printf
open Symbol
open Batteries

module Translate = struct
  type exp = unit
end

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

 **)

type expty = Types.t

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

let rec transDecl (tenv : Types.typeEnv) (venv : Types.valEnv) (decls : S.decl list) : (Types.typeEnv * Types.valEnv) =
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
            let t = Types.RECORD (rec_fields) in
            t
         | S.ArrayTy (pos, sym) ->
            begin match SymbolTable.look sym tenv with
            | Some (t) -> Types.ARRAY(t)
            | None -> raise_undef pos sym
            end
         end
       in
       trtype_decl (SymbolTable.enter name t' tenv) tl
  in
  let rec trfunc_decl tenv venv (decl : (Pos.t * S.funcdecl) list) : (Types.typeEnv * Types.valEnv) = match decl with
    | [] -> tenv, venv
    | (pos, {S.funName;S.fparams;S.fresult;S.fbody}) :: tl ->
       let args_t = List.map (fun param -> trfieldTy tenv param) fparams in
       let ret_t = match fresult with
         | None -> Types.UNIT
         | Some (t) -> get_type pos t tenv in
       let functype = Types.FuncType(List.map (fun (_, t) -> t) args_t, ret_t) in
       let venv' = SymbolTable.enter funName functype venv in (* binds func name *)
       let venv'' = List.fold_right (fun (name,t) table -> (* binds args *)
                       SymbolTable.enter name (Types.VarType(t)) table) args_t venv' in
       let body_t = transExp tenv venv'' fbody in
       if ret_t <> body_t then
         expect_type pos (Types.t_to_string ret_t) body_t
       else
         trfunc_decl tenv venv' tl
  in
  match decls with
  | [] -> tenv, venv
  | hd :: tl ->
     let tenv', venv' =
       begin match hd with
       | S.VarDecl(pos, name, decl_ty, init) ->
          let init_t = transExp tenv venv init in
          let declared_t =
            begin match decl_ty with
            | None ->
               if init_t = Types.NIL then
                 raise (TypeError(pos, "You must declare the type of variable " ^ (Symbol.to_string name)))
               else init_t
            | Some (decl_t) when get_type pos decl_t tenv = init_t -> init_t
            | Some (decl_t) -> expect_type pos (Symbol.to_string decl_t) init_t
            end in
          let venv' = SymbolTable.enter name (Types.VarType (declared_t)) venv in
          tenv, venv'
       | S.TypeDecl (lst) ->
          let name_t = List.map (fun (_, s, _) -> s, Types.NAME(s, ref None)) lst in
          let tenv' = List.fold_right (fun (name,t) table ->
                          SymbolTable.enter name t table) name_t tenv in
          trtype_decl tenv' lst, venv
       | S.FunctionDecl (lst) -> trfunc_decl tenv venv lst
       end in
     transDecl tenv' venv' tl

and transExp (tenv : Types.typeEnv) (venv : Types.valEnv) (expr : S.exp) : expty =
  let rec trvar (var : S.var) : expty =
    match var with
    | S.VarId (pos, sym) -> begin
        match SymbolTable.look sym venv with
        | None -> raise_undef pos sym
        | Some (Types.VarType(t)) -> t
        | Some (typ) -> expect_vtype pos (Symbol.to_string sym) typ
      end
    | S.VarField (pos, var1, sym) -> begin
        match trvar var1 with
        | Types.RECORD (lst) ->
           begin try List.assoc sym lst
                 with
                   Not_found -> raise_undef pos sym
           end
        | t -> expect_type pos "record" t

      end
    | S.VarSubscript(pos, var1, e) ->
       match trvar var1 with
       | Types.ARRAY (t) ->
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
       if left_ty == Types.INT && right_ty = Types.INT then
         Types.INT
       else raise (TypeError(pos, "Operator applied to non-integral types: " ^
                                    (Types.t_to_string left_ty) ^ " and " ^
                                      (Types.t_to_string right_ty)))
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
        | Some (Types.FuncType (arg_t, ret_t)) ->
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
           | Types.RECORD(lst) ->
              List.iter (fun (pos, sym, e) ->
                  (* first see if sym is in the record type. *)
                  match Types.record_find lst sym with
                  | None -> raise_undef pos sym
                  | Some (t) ->
                     (* 2. see if e's type matches declared type *)
                     let e_t = trexp e in
                     if t = e_t then ()
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
          let venv' = SymbolTable.enter v (Types.VarType(Types.INT)) venv in
          let body_t = transExp tenv venv' body in
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
       let tenv', venv' = transDecl tenv venv decl in
       transExp tenv' venv' body
    | S.Arr (pos, typ, size, init) ->
       begin match SymbolTable.look typ tenv with
       | Some(Types.ARRAY(t)) ->
          let size_t = trexp size in
          if size_t <> Types.INT then
            expect_type (S.get_exp_pos size) "int" size_t
          else
            let init_t = trexp init in
            if init_t <> t then
              expect_type (S.get_exp_pos init) (Types.t_to_string t) init_t
            else
              Types.ARRAY(t)
       | Some(other_t) ->
          expect_type pos "array" other_t
       | None ->
          raise_undef pos typ
       end
  in
  trexp expr

let transProg (e : S.exp) : unit =
  ignore(transExp Types.typeEnv Types.valEnv e)
