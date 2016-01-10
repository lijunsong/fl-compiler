open Pos

type id = string

type symbol = string

type exp =
  | String of Pos.t * string
  | Int of Pos.t * int
  | Nil of Pos.t
  | Var of Pos.t * var
  | Op of Pos.t * op * exp * exp (** op * left * right *)
  | Assign of Pos.t * var * exp
  | Call of Pos.t * symbol * exp list
  | Record of Pos.t * symbol * (Pos.t * symbol * exp) list (** type * fields *)
  | Seq of exp list (** This construct does not need Pos.t *)
  | If of Pos.t * exp * exp * exp option (** if test then else *)
  | While of Pos.t * exp * exp (** while test body *)
  | For of Pos.t * symbol * exp * exp * exp (** var * lo * hi * body *)
  | Break of Pos.t
  | Let of Pos.t * decl list * exp
  | Arr of Pos.t * symbol * exp * exp (** type * size * init *)
 and decl =
   | FunctionDecl of (Pos.t * funcdecl) list (** a list for functions *)
   | VarDecl of Pos.t * symbol * symbol option * exp (** name * type * init *)
   | TypeDecl of (Pos.t * symbol * ty) list (** name * type *)
 and op =
   | OpPlus | OpMinus | OpTimes | OpDiv | OpEq | OpNeq
   | OpLt | OpGt | OpLe | OpGe
 and var =
   | VarId of id (** id in lvalue *)
   | VarField of var * id (** lvalue . id *)
   | VarSubscript of var * exp (** lvalue [ exp ] *)
 and ty =
   | NameTy of Pos.t * symbol
   | RecordTy of fieldTy list
   | ArrayTy of Pos.t * symbol
 and fieldTy = {
     fldName: symbol;
     ty: symbol;
     pos: Pos.t;
   }
 and funcdecl = {
     funcName: symbol;
     params: fieldTy list;
     result: symbol option;
     body: exp;
   }
