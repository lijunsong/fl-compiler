open Sexplib.Std
open Sexplib.Conv
open Symbol

type symbol = Symbol.t with sexp

type pos_t = Pos.t sexp_opaque with sexp

type exp =
  | String of pos_t * string
  | Int of pos_t * int
  | Nil of pos_t
  | Var of pos_t * var
  | Op of pos_t * op * exp * exp (** op * left * right *)
  | Assign of pos_t * var * exp
  | Call of pos_t * symbol * exp list
  | Record of pos_t * symbol * (pos_t * symbol * exp) list (** construct a record value: type * fields *)
  | Seq of pos_t * exp list (** This construct does not need pos_t *)
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
   } with sexp

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
