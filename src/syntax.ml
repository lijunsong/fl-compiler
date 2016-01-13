open Sexplib.Std
open Sexplib.Conv

open Pos

type id = string with sexp

type symbol = string with sexp

type pos_t = Pos.t sexp_opaque with sexp

type exp =
  | String of pos_t * string
  | Int of pos_t * int
  | Nil of pos_t
  | Var of pos_t * var
  | Op of pos_t * op * exp * exp (** op * left * right *)
  | Assign of pos_t * var * exp
  | Call of pos_t * symbol * exp list
  | Record of pos_t * symbol * (pos_t * symbol * exp) list (** type * fields *)
  | Seq of exp list (** This construct does not need pos_t *)
  | If of pos_t * exp * exp * exp option (** if test then else *)
  | While of pos_t * exp * exp (** while test body *)
  | For of pos_t * symbol * exp * exp * exp (** var * lo * hi * body *)
  | Break of pos_t
  | Let of pos_t * decl list * exp
  | Arr of pos_t * symbol * exp * exp (** type * size * init *)
 and decl =
   | FunctionDecl of (pos_t * funcdecl) list (** a list for functions *)
   | VarDecl of pos_t * symbol * symbol option * exp (** name * type * init *)
   | TypeDecl of (pos_t * symbol * ty) list (** name * type *)
 and op =
   | OpPlus | OpMinus | OpTimes | OpDiv | OpEq | OpNeq
   | OpLt | OpGt | OpLe | OpGe
 and var =
   | VarId of id (** id in lvalue *)
   | VarField of var * id (** lvalue . id *)
   | VarSubscript of var * exp (** lvalue [ exp ] *)
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
     funcName: symbol;
     params: fieldTy list;
     result: symbol option;
     body: exp;
   } with sexp
