/* Ocamlyacc Parser for Stop */

%{ 
    open Ast
    open Core.Std 
    module E = Exceptions
    let lambda_num = ref 0
%}

%token DOT COMMA SEMI COLON LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT CARET MODULO
%token INCREMENT DECREMENT
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token IF ELSE FOR WHILE BREAK CONTINUE
%token ARROW FATARROW
%token RETURN
%token FINAL
%token PUBLIC PRIVATE ANON
%token SPEC CLASS METHOD
%token MATCH CASE 
%token TYPE VAR THIS
%token DEF EXTENDS 
%token EOF

/* Processor Directives */

%token INCLUDE
%token MODULE

/* Primitive Types */

%token INT FLOAT BOOL CHAR UNIT
%token <string> TYPE_ID

/* Literals */

%token <int> INT_LIT
%token <float> FLOAT_LIT
%token <char> CHAR_LIT
%token <string> STRING_LIT
%token <string> ID

/* Precedence Rules */

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left AND OR
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%right NOT NEG
%right RBRACKET
%left LBRACKET
%left INCREMENT DECREMENT
%right DOT
%right ARROW

%start program
%type <Ast.program> program

%%

/* Context-Free Grammar */
/* -------------------- */

program:
    constituents EOF { Program(List.rev $1.includes, List.rev $1.specs, 
                                List.rev $1.cdecls, List.rev $1.fdecls) } 

constituents:
    { { 
        includes = [];
        specs = [];
        cdecls = [];
        fdecls = [];
    } }
  | constituents include_stmt { {
        includes = $2 :: $1.includes;
        specs = $1.specs; 
        cdecls = $1.cdecls; 
        fdecls = $1.fdecls; 
    } }
  | constituents sdecl { {
        includes = $1.includes;
        specs = $2 :: $1.specs; 
        cdecls = $1.cdecls; 
        fdecls = $1.fdecls; 
    } }
  | constituents cdecl { {
        includes = $1.includes;
        specs = $1.specs; 
        cdecls = $2 :: $1.cdecls; 
        fdecls = $1.fdecls; 
    } }
  | constituents fdecl { {
        includes = $1.includes;
        specs = $1.specs; 
        cdecls = $1.cdecls; 
        fdecls = $2 :: $1.fdecls; 
    } }

/* Includes */
/* -------- */

include_stmt:
    INCLUDE STRING_LIT          { Include($2) }

/* Functions */
/* --------- */

fdecl:
    DEF ID ASSIGN LPAREN formals_opt RPAREN COLON datatype LBRACE stmts RBRACE { { 
        fname = $2;
        ftype = Functiontype(snd $5, $8);
        return_t = $8;
        formals = fst $5;
        body = $10;
        scope = Public;
        overrides = false;
        root_cname = None;
    } }

/* Specs */
/* ----- */

sdecl:
    SPEC TYPE_ID LBRACE RBRACE { { 
            sname = $2;
    } }

/* Classes */
/* ------- */

cdecl:
    CLASS TYPE_ID ASSIGN LBRACE cbody RBRACE { {
        cname = $2;
        extends = NoParent;
        cbody = $5;
    } }

cbody:
    /* nothing */ { {
        fields = [];
        methods = [];
    } }
  | cbody field { {
        fields = $2 :: $1.fields;
        methods = $1.methods;
    } }
  | cbody cfdecl { {
        fields = $1.fields;
        methods = $2 :: $1.methods;
    } }

cfdecl:
    scope DEF ID ASSIGN LPAREN formals_opt RPAREN COLON datatype LBRACE stmts RBRACE { { 
            fname = $3;
            ftype = Functiontype(snd $6, $9);
            return_t = $9;
            formals = fst $6;
            body = $11;
            scope = $1;
            overrides = false;
            root_cname = None;
    } }

/* Datatypes */
/* --------- */

datatype:
    type_tag        { Datatype($1) }
  | array_type      { $1 }
  | function_type   { $1 }

type_tag:
    primitive       { $1 }
  | object_type     { $1 }

/* AST Datatype */

primitive:
    INT             { Int_t }
  | FLOAT           { Float_t }
  | CHAR            { Char_t }
  | BOOL            { Bool_t }
  | UNIT            { Unit_t }

object_type:
    TYPE_ID { Object_t($1) }

/* AST Arraytype */

array_type:
    type_tag LBRACKET brackets RBRACKET { Arraytype($1, $3) }

brackets:
    /* nothing */              { 1 }
  | brackets RBRACKET LBRACKET { $1 + 1 }

/* AST Functiontype */

/* Type1->Type2 is shorthand for (Type1)->Type2 */
/* NOTE: ARROW is right-associative */
function_type:
    LPAREN formal_dtypes_list RPAREN ARROW datatype     { Functiontype($2, $5) }
  | datatype ARROW datatype                             { Functiontype([$1], $3) }

/* Fields */
/* ------ */

field:
    scope VAR ID COLON datatype SEMI { Field($1, $3, $5) }
      
/* Formals and Actuals */
/* ------------------- */

/* Formal Datatypes -- Nameless for Function Types */
formal_dtypes_list:
    formal_dtype                            { [$1] }
  | formal_dtypes_list COMMA formal_dtype   { $3::$1 }

formal_dtype:
    datatype       { $1 }

/* Formals -- Names & Datatypes for Functions */
/* Returns (f, t), where f = list of formal and t = list of data_t */
formals_opt:
    /* nothing */               { ([], []) }
  | formal_list                 { (List.rev (fst $1), List.rev (snd $1))  }

formal_list:
    formal                      { ([fst $1], [snd $1])  }
  | formal_list COMMA formal    { (fst $3 :: fst $1), (snd $3 :: snd $1) }

formal:
    ID COLON datatype           { (Formal($1, $3), $3) }

/* Actuals -- Exprs evaluated for Function Calls */

actuals_opt:
    /* nothing */               { [] }
  | actuals_list                { List.rev $1 }

actuals_list:
    expr                        { [$1] }
  | actuals_list COMMA expr     { $3::$1 }

/* Scope */
/* ----- */

scope:
    /* nothing */       { Public }
  | PUBLIC              { Public }
  | PRIVATE             { Private }

/* Literals */
/* -------- */

literals:
      INT_LIT           { IntLit($1) }
    | FLOAT_LIT         { FloatLit($1) }
    | TRUE              { BoolLit(true) }
    | FALSE             { BoolLit(false) }
    | CHAR_LIT          { CharLit($1) }
    | STRING_LIT        { StringLit($1) }
    | function_literal  { $1 }
    | ID                { Id($1) }
    | THIS              { This }

function_literal:
    ANON LPAREN formals_opt RPAREN COLON datatype LBRACE stmts RBRACE { 
        lambda_num := !lambda_num + 1;
        FunctionLit({
            fname = "@" ^ string_of_int !lambda_num;
            ftype = Functiontype(snd $3, $6);
            return_t = $6;
            formals = fst $3;
            body = $8;
            scope = Private;
            overrides = false;
            root_cname = None;
        }) 
    }

bracket_args:
    LBRACKET expr       { [$2] }
  | bracket_args RBRACKET LBRACKET expr { $4 :: $1 }

/* Statements */
/* ---------- */

stmts:
    | stmt_list             { List.rev $1 }
    
stmt_list:
      stmt                  { [$1] }
    | stmt_list stmt        { $2::$1 }

stmt:
      expr SEMI                                 { Expr($1) }
    | RETURN SEMI                               { Return(Noexpr) }
    | RETURN expr SEMI                          { Return($2) }
    | LBRACE stmts RBRACE                       { Block($2) } 
    | IF LPAREN expr RPAREN stmt ELSE stmt      { If($3, $5, $7) }
    | WHILE LPAREN expr RPAREN stmt             { While($3, $5) }
    | VAR ID COLON datatype SEMI                { Local($2, $4, Noexpr) }
    | VAR ID ASSIGN expr SEMI                   { Local($2, Any, $4) }
    | VAR ID COLON datatype ASSIGN expr SEMI    { Local($2, $4, $6) }
    | IF LPAREN expr RPAREN stmt %prec NOELSE   { If($3, $5, Block([])) }
    | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt { For($3, $5, $7, $9) }
    | BREAK SEMI                                { Break }
    | CONTINUE SEMI                             { Continue }

/* Expressions */
/* ----------- */

expr_opt:
      /* nothing */         { Noexpr }
    | expr                  { $1 }

expr:
      literals                          { $1 }
    | expr INCREMENT                    { Binop($1, Add, IntLit(1)) }
    | expr DECREMENT                    { Binop($1, Sub, IntLit(1)) }
    | expr PLUS     expr                { Binop($1, Add, $3) }
    | expr MINUS    expr                { Binop($1, Sub, $3) }
    | expr TIMES    expr                { Binop($1, Mult, $3) }
    | expr DIVIDE   expr                { Binop($1, Div, $3) }
    | expr MODULO   expr                { Binop($1, Modulo, $3) }
    | expr EQ       expr                { Binop($1, Equal, $3) }
    | expr NEQ      expr                { Binop($1, Neq, $3) }
    | expr LT       expr                { Binop($1, Less, $3) }
    | expr LEQ      expr                { Binop($1, Leq, $3) }
    | expr GT       expr                { Binop($1, Greater, $3) }
    | expr GEQ      expr                { Binop($1, Geq, $3) }
    | expr AND      expr                { Binop($1, And, $3) }
    | expr OR       expr                { Binop($1, Or, $3) }
    | expr ASSIGN   expr                { Assign($1, $3) }
    | expr DOT      expr                { ObjAccess($1, $3) }
    | expr bracket_args RBRACKET        { ArrayAccess($1, List.rev $2) }
    | MINUS expr %prec NEG              { Unop(Neg, $2) } 
    | NOT expr                          { Unop(Not, $2) }
    | LPAREN expr RPAREN                { $2 }
    | ID LPAREN actuals_opt RPAREN      { Call($1, $3) }
    | type_tag bracket_args RBRACKET LPAREN RPAREN { ArrayCreate (Datatype($1), List.rev $2) }

%%
