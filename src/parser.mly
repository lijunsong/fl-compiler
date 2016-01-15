%{
module S = Syntax
open Pos

(** [get_pos n1 n2] This fuction is used in a rule to get the start
    position of item n1 and end position of item n2 *)
let get_pos p1 p2 =
  { Pos.start_p = Parsing.rhs_start_pos p1;
    Pos.end_p = Parsing.rhs_end_pos p2 }

let _ = Parsing.set_trace true

%}

/* token convention:
 * Tokens that start with one capital letter have semantic values
 * Tokens that contains only capital letters have no semantic values
 */
%token <string> Id
%token <string> String
%token <int> Int

%token ARRAY BREAK DO ELSE END FOR FUNCTION IF
%token IN LET NIL OF THEN TO TYPE VAR WHILE

%token LP RP LBRACE RBRACE LBRACKET RBRACKET COMMA COLON SEMICOLON DOT
%token PLUS MINUS TIMES DIV EQ NEQ LT GT LE GE AND OR ASSIGN
%token NIL

%token EOF

%left AND OR
%left LBRACKET RBRACKET LBRACE RBRACE LP RP
%left PLUS MINUS
%left TIMES DIV
%nonassoc EQ NEQ LT GT LE GE
%right ASSIGN
%left ELSE

%nonassoc UMINUS /* last one: highest precedence */

%start prog
%type <Syntax.exp> prog

%%
prog:
  expr EOF { $1 }
;

expr:
 | String { S.String(get_pos 1 1, $1) }
 | Int { S.Int(get_pos 1 1, $1) }
 | NIL { S.Nil(get_pos 1 1) }
 | lvalue { S.Var(get_pos 1 1, $1)}
 | MINUS expr %prec UMINUS
   { S.Op(get_pos 1 1, S.OpMinus,
     S.Int(Pos.dummy, 0), $2)}
 | op { $1 }
 | lvalue ASSIGN expr
   { S.Assign(get_pos 1 1, $1, $3) }
 | Id LP expr_list RP
   { S.Call(get_pos 1 1, $1, $3) }
 | LP expr_seq RP
   { S.Seq $2 }
 | Id LBRACE field_list RBRACE
   { S.Record(get_pos 1 1, $1, $3) }
 | Id LBRACKET expr RBRACKET OF expr
   { S.Arr(get_pos 1 1, $1, $3, $6) }
 | IF expr THEN expr
   { S.If(get_pos 1 1, $2, $4, None) }
 | IF expr THEN expr ELSE expr
   { S.If(get_pos 1 1, $2, $4, Some $6) }
 | FOR Id ASSIGN expr TO expr DO expr
   { S.For(get_pos 1 1, $2, $4, $6, $8) }
 | WHILE expr DO expr
   { S.While(get_pos 1 1, $2, $4) }
 | BREAK { S.Break(get_pos 1 1) }
 | LET decl_list IN expr_seq END
   { S.Let(get_pos 1 1, $2, S.Seq($4)) }
;

op:
 | expr AND expr
   { S.If(get_pos 2 2, $1, $3, Some (S.Int(Pos.dummy, 0))) }
 | expr OR expr
   { S.If(get_pos 2 2, $1, S.Int(Pos.dummy, 1), Some $3) }
 | expr PLUS expr { S.Op(get_pos 2 2, S.OpPlus, $1, $3) }
 | expr MINUS expr { S.Op(get_pos 2 2, S.OpMinus, $1, $3) }
 | expr TIMES expr { S.Op(get_pos 2 2, S.OpTimes, $1, $3) }
 | expr DIV expr { S.Op(get_pos 2 2, S.OpDiv, $1, $3) }
 | expr EQ expr { S.Op(get_pos 2 2, S.OpEq, $1, $3) }
 | expr NEQ expr { S.Op(get_pos 2 2, S.OpNeq, $1, $3) }
 | expr LT expr { S.Op(get_pos 2 2, S.OpLt, $1, $3) }
 | expr GT expr { S.Op(get_pos 2 2, S.OpGt, $1, $3) }
 | expr LE expr { S.Op(get_pos 2 2, S.OpLe, $1, $3) }
 | expr GE expr { S.Op(get_pos 2 2, S.OpGe, $1, $3) }

decl_list: rev_decl_list { List.rev $1 }
;

rev_decl_list:
  | declation { [$1] }
  | rev_decl_list declation
    {
       (* If item 2 is FunctionDecl or TypeDecl, it is a list of length 1*)
       match $1, $2 with
         | S.TypeDecl (lst) :: tail, S.TypeDecl (item) ->
           S.TypeDecl(lst @ item) :: tail
         | S.FunctionDecl(lst) :: tail, S.FunctionDecl (item) ->
           S.FunctionDecl(lst @ item) :: tail
         | _ -> $2 :: $1
    }
;

declation:
  | TYPE Id EQ type_decl  { S.TypeDecl([get_pos 2 2, $2, $4]) }
  | VAR Id ASSIGN expr { S.VarDecl(get_pos 2 2, $2, None, $4) }
  | VAR Id COLON Id ASSIGN expr { S.VarDecl(get_pos 2 2, $2, Some $4, $6) }
  | FUNCTION Id LP type_fields_opt RP EQ expr
    { S.FunctionDecl([get_pos 2 2,
                      {S.funName=$2;S.fparams=$4;S.fresult=None;S.fbody=$7}])}
  | FUNCTION Id LP type_fields_opt RP COLON Id EQ expr
    { S.FunctionDecl([get_pos 2 2,
                      {S.funName=$2;S.fparams=$4;S.fresult=Some $7;S.fbody=$9}])}
;

type_decl:
  | Id { S.NameTy(get_pos 1 1, $1) }
  | LBRACE type_fields_opt RBRACE
    { S.RecordTy($2) }
  | ARRAY OF Id
    { S.ArrayTy(get_pos 3 3, $3) }
;

type_fields_opt:
  | /* empty */  { [] }
  | type_fields  { List.rev $1 }
;

type_fields:
  | type_field { [$1] }
  | type_fields COMMA type_field { $3 :: $1 }
;

type_field:
  Id COLON Id { {S.fldName = $1; S.ty = $3; S.pos = get_pos 1 1} }
;

field_list:
 | /* empty */ { [ ] }
 | rev_field_list { List.rev $1 }
;

rev_field_list:
  | Id EQ expr { [get_pos 1 3, $1, $3] }
  | rev_field_list COMMA Id EQ expr
    { (get_pos 3 5, $3, $5) :: $1 }

expr_list:
  | /* empty */ { [ ] }
  | rev_expr_list { List.rev $1 }
;
rev_expr_list:
  | expr { [$1] }
  | rev_expr_list COMMA expr { $3 :: $1 }
;

expr_seq:
  | /* empty */ { [] }
  | rev_expr_seq { List.rev $1 }
;
rev_expr_seq:
  | expr { [$1] }
  | rev_expr_seq SEMICOLON expr { $3 :: $1 }

lvalue:
 | Id { S.VarId($1) }
 | lvalue DOT Id { S.VarField($1, $3) }
 | lvalue LBRACKET expr RBRACKET { S.VarSubscript($1, $3) }
;
