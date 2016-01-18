module S = Syntax
open Printf

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
  raise (TypeError(pos, "Expected type " ^ expect ^ "but got " ^ (Types.t_to_string actual)))

let rec transDecl (tenv : typeEnv) (venv : valEnv) (decls : S.decl list) : (typeEnv * valEnv) =
  let trfieldTy (te : typeEnv) fld =
    match SymbolTable.look fld.ty te with
    | None -> raise_undef fld.pos fld.ty
    | Some (t) -> (fld.fldName, t)
  in
  (* Translate a list of TypeDecl *)
  let rec trtype_decl tenv venv (decl : (Pos.t * Symbol.t * S.ty) list) : (typeEnv * valEnv) = match decl with
    | [] -> tenv, venv
    | (pos, name, ty) :: tl ->
       let ty' =
         begin match ty with
         | S.NameTy (pos, typeid) ->
            begin match SymbolTable.look typeid tenv with
            | Some (t) -> t
            | None -> raise_undef pos typeid
            end
         | S.RecordTy (fld_list) ->
            let placeholder = ref None in
            let tenv' = SymbolTable.enter name (Types.NAME(name, placeholder)) in
            let rec_fields = List.map (fun fld -> trfieldTy tenv' fld) fld_list in
            let t = Types.RECORD (rec_fields) in
            placeholder := t;
            t
         | S.ArrayTy (pos, sym) ->
            begin match SymolTable.look sym tenv with
            | Some (t) -> Types.ARRAY(t)
            | None -> raise_undef pos sym
            end
         end
       in
       trtype_decl (SymbolTable.enter name ty' tenv) venv tl
  in
  let rec trfunc_decl tenv venv (decl : (Pos.t * S.funcdecl list)) : (typeEnv * valEnv) = match decl with
    | [] -> tenv, venv
    | (pos, {funName;fparams;fresult;fbody}) :: tl ->
       let args_t = List.map (fun param -> trfieldTy tenv param) fparams in
       let ret_t = match fresult with
         | None -> Type.UNIT
         | Some (t) -> t in
       let functype = Types.FuncType(args_t, ret_t) in
       let venv' = SymbolTable.enter funName functype venv in
       let body_t = transExp tenv venv' fbody in
       if ret_t <> body_t then
         expect_type pos (Types.t_to_string ret_t) body_t
       else
         trfunc_decl tenv venv' tl
  in
  match decls with
  | [] -> tenv, venv
  | hd :: tl ->
     begin match hd with
     | VarDecl(pos, name, typ, init) ->
        let init_t = transExp tenv venv init in
        let declared_t =
          begin match typ with
          | None -> init_t
          | Some (typ_t) when typ_t = init_t -> init_t
          | Some (typ_t) -> expect_type pos (Types.t_to_string typ_t) init_t
          end in
        let venv' = SymbolTable.enter name declared_t venv in
        transDecl tenv venv' tl
     | TypeDecl (lst) -> trtype_decl tenv venv lst
     | FunctionDecl (lst) -> trfunc_decl tenv venv lst
     end

and transExp (tenv : typeEnv) (venv : valEnv) (expr : S.exp) : expty =
  let rec trvar (var : S.Var) : expty =
    match var with
    | S.VarId (pos, sym) -> begin
        match SymbolTable.lookup sym venv with
        | None -> raise_undef pos sym
        | Some (t) -> t
      end
    | S.VarField (pos, var1, sym) -> begin
        match trvar var1 with
        | Types.RECORD (lst) ->
           try List.assoc sym lst
           with
             Not_found -> raise_undef pos sym
           | t -> expect_type pos "record" t
      end
    | S.VarSubscript(pos, var1, e) ->
       match trvar var1 with
       | Types.ARRAY (t) ->
          begin match trexp e with
          | INT -> t
          | t' -> expect_type pos "int" t'
          end
       | t' -> expect_type pos "array" t'
  and trexp (exp : S.exp) : expty =
    match exp with
    | S.Var (_, var) -> trvar var
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
         expect_type pos (Types.to_string left_ty) right_ty
       else Types.UNIT
    | S.Call (pos, f, args) -> begin
        let f_type = SymbolTable.lookup f venv with
        | Some (Types.VarType(_)) ->
           raise (TypeError(pos, (Symbol.to_string f) ^ " is not applicable"))
        | Some (Types.FuncType (arg_t, ret_t)) ->
           let args_type = List.map trexp args in
           if args_type = arg_t then ret_t
           else
             expect_type pos
                         (String.join ", " List.map Types.t to_string arg_t)
                         (String.join "," (List.map Types.t_to_string args_type))
        | None -> raise_undef pos f
      end
    | S.Record (pos, record, fields) ->
       begin match SymbolTable.lookup record tenv with
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
                     else expect_type pos (Symbol.t_to_string t) e_t
                ) lst
           | _ -> expect_type pos "record" record);
           record
       end
    | S.Seq (lst) ->
       let rec trexp_list = function
         | [] -> Types.UNIT
         | hd :: [] -> trexp hd
         | hd :: tl -> ignore(trexp hd); trexp_list tl
       in
       trexp_list lst
    | S.If (pos, tst, thn, None) ->
       let tst_t = trexp tst in
       if tst_t <> Types.INT then
         expect_type (get_exp_pos tst) "int" tst_t
       else
         begin match trexp thn with
         | Types.UNIT -> Types.UNIT
         | thn_t -> expect_type (get_exp_pos thn) "unit" thn_t
         end
    | S.If (pos, tst, thn, Some (els)) ->
       let tst_t = trexp tst in
       if tst_t <> Types.INT then
         expect_type (get_exp_pos tst) "int" tst_t
       else
         let thn_t = trexp thn in
         let els_t = trexp els in
         if thn_t = els_t then thn_t
         else expect_type (get_exp_pos els) (Types.t_to_string thn_t) els_t
    | S.While (pos, tst, body) ->
       let tst_t = trexp tst in
       if tst_t <> Types.INT then
         expect_type (get_exp_pos tst) "int" tst_t
       else let body_t = trexp body in
            if body_t <> Types.UNIT then
              expect_type (get_exp_pos body) "unit" body_t
            else Types.UNIT
    | S.For (pos, v, lo, hi, body) ->
       (** For exp implicitly binds v to the type of lo/hi in the body *)
       begin match trexp lo, trexp hi with
       | Types.INT, Types.INT ->
          let venv' = SymbolTable.enter v Types.INT venv in
          let body_t = transExp tenv venv' body in
          if body_t <> Types.UNIT then
            expect_type (get_exp_pos body) "unit" body_t
          else body_t
       | lo_t, Types.INT ->
          expect_type (get_exp_pos lo) "int" lo_t
       | Types.INT, hi_t ->
          expect_type (get_exp_pos hi) "int" lo_t
       | lo_t, _ ->
          expect_type (get_exp_pos lo) "int" lo_t
       end
    | S.Let (pos, decl, body) ->
       let tenv', venv' = transDecl tenv venv decl in
       transExp tenv' venv' body
    | S.Arr (pos, typ, size, init) ->
       begin match SymbolTable.look typ tenv with
       | Some(ARRAY(t)) ->
          let size_t = trexp size in
          if size_t <> Types.INT then
            expect_type (get_exp_pos size) "int" size_t
          else
            let init_t = trexp init in
            if init_t <> t then
              expect_type (get_exp_pos init) (Types.t_to_string t) init_t
            else
              ARRAY(t)
       | Some(other_t) ->
          expect_type pos "array" other_t
       | None ->
          raise_undef pos typ
       end
  in
  trexp expr
