(* Utils *)
(* ----- *)

(* Collection of utilities used in other modules (e.g. pretty printing, tokenization, etc. *)

open Ast
open Parser
open Sast
open Core.Std

module E = Exceptions

(* Tokens *)
(* ------ *)

let string_of_token = function
    SEMI            -> "SEMI"
  | LPAREN          -> "LPAREN"
  | RPAREN          -> "RPAREN"
  | LBRACE          -> "LBRACE"
  | RBRACE          -> "RBRACE"
  | LBRACKET        -> "LBRACKET"
  | RBRACKET        -> "RBRACKET"
  | COMMA           -> "COMMA"
  | COLON           -> "COLON"
  | INCREMENT       -> "INCREMENT"
  | DECREMENT       -> "DECREMENT"
  | PLUS            -> "PLUS"
  | MINUS           -> "MINUS"
  | TIMES           -> "TIMES"
  | DIVIDE          -> "DIVIDE"
  | ASSIGN          -> "ASSIGN"
  | NOT             -> "NOT"
  | CARET           -> "CARET"
  | MODULO          -> "MODULO"
  | EQ              -> "EQ"
  | NEQ             -> "NEQ"
  | LT              -> "LT"
  | LEQ             -> "LEQ"
  | GT              -> "GT"
  | GEQ             -> "GEQ"
  | TRUE            -> "TRUE"
  | FALSE           -> "FALSE"
  | AND             -> "AND"
  | OR              -> "OR"
  | IF              -> "IF"
  | ELSE            -> "ELSE"
  | FOR             -> "FOR"
  | WHILE           -> "WHILE"
  | BREAK           -> "BREAK"
  | CONTINUE        -> "CONTINUE"
  | RETURN          -> "RETURN"
  | FINAL           -> "FINAL"
  | INCLUDE         -> "INCLUDE"
  | MODULE          -> "MODULE"
  | DOT             -> "DOT"
  | SPEC            -> "SPEC"
  | CLASS           -> "CLASS"
  | METHOD          -> "METHOD"
  | ARROW           -> "ARROW"
  | FATARROW        -> "FATARROW"
  | PUBLIC          -> "PUBLIC"
  | PRIVATE         -> "PRIVATE"
  | ANON            -> "ANON"
  | MATCH           -> "MATCH"
  | CASE            -> "CASE"
  | INT             -> "INT"
  | FLOAT           -> "FLOAT"
  | BOOL            -> "BOOL"
  | CHAR            -> "CHAR"
  | UNIT            -> "UNIT"
  | TYPE            -> "TYPE"
  | VAR             -> "VAR"
  | THIS            -> "THIS"
  | DEF             -> "DEF"
  | EXTENDS         -> "EXTENDS"
  | EOF             -> "EOF"
  | INT_LIT(_)      -> "INT_LIT"
  | FLOAT_LIT(_)    -> "FLOAT_LIT"
  | CHAR_LIT(_)     -> "CHAR_LIT"
  | STRING_LIT(_)   -> "STRING_LIT"
  | ID(_)           -> "ID"
  | TYPE_ID(_)      -> "TYPE_ID"

let rec token_list_to_string = function
        (token, _) :: tail -> 
            string_of_token token ^ " " ^
            token_list_to_string tail
      | [] -> "\n"

(* Parsing Error Functions *)
(* ----------------------- *)

let error_string_of_file filename =
    if filename = "" 
    then "Stdin"
    else "File \"" ^ filename ^ "\""

let error_string_of_cnum cnum token =
    string_of_int cnum ^ "~" 
    ^ string_of_int (cnum + String.length (string_of_token token))

(* Pretty-printing Functions *)
(* ------------------------- *)

let string_of_op = function 
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Modulo -> "%"
  | Ast.Equal -> "=="
  | Neq -> "!="
  | Ast.Less -> "<"
  | Leq -> "<="
  | Ast.Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let string_of_primitive = function 
    Int_t -> "Int"
  | Float_t -> "Float"
  | Bool_t -> "Bool"
  | Char_t -> "Char"
  | Unit_t -> "Unit"
  | Object_t(s) -> s

let rec print_brackets = function
    1 -> "[]"
  | i -> "[]" ^ print_brackets (i - 1)

let rec string_of_datatype = function
    Datatype(p) -> string_of_primitive p
  | Arraytype(p, i) -> string_of_primitive p ^ print_brackets i
  | Functiontype(formal_dtypes, rtype) -> 
        "(" ^
        String.concat ~sep:"," (List.map ~f:string_of_datatype formal_dtypes) ^ ")->" ^
        string_of_datatype rtype
  | Any -> "Any"

let string_of_scope = function
    Public -> "public"
  | Private -> "private"

(* type formal = Formal of datatype * string *)
let string_of_formal = function
    Formal(s, data_t) -> s ^ ":" ^ string_of_datatype data_t 
  | Many(data_t) -> "Many :" ^ string_of_datatype data_t

let string_of_formal_name = function
    Formal(s, _) -> s
  | Many(_) -> "Many"

let string_of_field = function
    Field(scope, s, data_t) -> 
        "\t" ^ string_of_scope scope ^ " " ^ s ^ ":" 
        ^ string_of_datatype data_t ^ ";\n"
  
(* Take a function that returns a string and make it tab the string *)
let prepend_tab f = fun s -> "\t" ^ f s

let rec string_of_method m =
    "\t" ^ string_of_scope m.scope ^ " def " ^ m.fname ^ " = (" ^
    String.concat ~sep:", " (List.map ~f:string_of_formal m.formals) ^
    "):" ^ string_of_datatype m.return_t ^ "{\n" ^ 
    String.concat ~sep:"" (List.map ~f:(prepend_tab string_of_stmt) m.body) ^
    "\t}\n"

and string_of_fdecl f =
    "function" ^ " " ^ f.fname ^ " = (" ^
    String.concat ~sep:", " (List.map ~f:string_of_formal f.formals) ^
    "):" ^ string_of_datatype f.return_t ^ "{\n" ^ 
    String.concat ~sep:"" (List.map ~f:string_of_stmt f.body) ^
    "}\n"

and string_of_expr = function
    IntLit(i) -> string_of_int i
  | FloatLit(f) -> string_of_float f
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | CharLit(c) -> String.make 1 c
  | StringLit(s) -> "\"" ^ s ^ "\""
  | FunctionLit(f) ->
        f.fname ^ "(" ^ 
        String.concat ~sep:", " (List.map ~f:string_of_formal f.formals) ^ "):" ^
        string_of_datatype f.return_t ^ "{\n" ^ 
        String.concat ~sep:"" (List.map ~f:(prepend_tab string_of_stmt) f.body) ^
        "\t}"
  | Id(i) -> i
  | Binop(e1, op, e2) ->
        string_of_expr e1 ^ " " ^ string_of_op op ^ " " ^ string_of_expr e2
  | Assign(e1, e2) -> string_of_expr e1 ^ " = " ^ string_of_expr e2
  | Unop(op, e1) ->
        string_of_uop op ^ " " ^ string_of_expr e1
  | Call(s, e_l) -> s ^ "(" ^ String.concat ~sep:", " (List.map ~f:string_of_expr e_l) ^ ")"
  | ObjAccess(e1, e2) -> string_of_expr e1 ^ "." ^ string_of_expr e2
  | ArrayAccess(e, e_l) -> 
        string_of_expr e ^ "[" ^ String.concat ~sep:"][" (List.map ~f:string_of_expr e_l) ^ "]"
  | ArrayCreate(d, e_l) ->
        string_of_datatype d ^ "[" ^ String.concat ~sep:"][" (List.map ~f:string_of_expr e_l) ^ "]"
  | This -> "this"
  | Noexpr -> ""

and string_of_stmt = function
    Block(stmts) -> 
        "{\n" ^ String.concat ~sep:"" (List.map ~f:string_of_stmt stmts) ^ "}\n"
  | _ as stmt -> 
        prepend_tab string_of_stmt_helper stmt 

and string_of_stmt_helper = function
    Block(_) -> raise (E.UtilsError("Encountered Block in string_of_stmt helper"))
  | Expr(expr) -> string_of_expr expr ^ ";\n"
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s1 
                        ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) -> "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " 
                            ^ string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | Break               -> "break;\n"
  | Continue            -> "continue;\n" 
  | Local(s, dtype, e) -> ( match e with 
          Noexpr -> "var " ^ s ^ ":" ^ string_of_datatype dtype ^ ";\n"
        | _ -> "var " ^ s ^ ":" ^ string_of_datatype dtype ^ " = " ^ string_of_expr e ^ ";\n" )
            
let string_of_include = function
    Include(s) -> "#include \"" ^ s ^ "\"\n"

let string_of_spec spec =
    "spec " ^ spec.sname ^ " {\n" ^ "}\n"

let string_of_cdecl cdecl = match cdecl.extends with
    NoParent ->
        "class " ^ cdecl.cname ^ " {\n" ^
        String.concat ~sep:"" (List.map ~f:string_of_field cdecl.cbody.fields) ^
        String.concat ~sep:"" (List.map ~f:string_of_method cdecl.cbody.methods) ^
        "}\n"
    | Parent(s) ->
        "class " ^ cdecl.cname ^ " extends " ^ s ^ " {\n" ^
        String.concat ~sep:"" (List.map ~f:string_of_field cdecl.cbody.fields) ^
        String.concat ~sep:"" (List.map ~f:string_of_method cdecl.cbody.methods) ^
        "}\n"

let string_of_program = function
    Program(includes, specs, cdecls, fdecls) -> 
        String.concat ~sep:"\n" (List.map ~f:string_of_include includes) ^ "\n" ^
        String.concat ~sep:"\n" (List.map ~f:string_of_spec specs) ^ "\n" ^
        String.concat ~sep:"\n" (List.map ~f:string_of_cdecl cdecls) ^ "\n" ^
        String.concat ~sep:"\n" (List.map ~f:string_of_fdecl fdecls)

(* SAST Printing Functions *)
(* ======================= *)

let rec string_of_bracket_sexpr = function
    [] -> ""
  | head :: tail  -> "[" ^ (string_of_sexpr head) ^ "]" ^ (string_of_bracket_sexpr tail)

and string_of_sarray_primitive = function
    [] -> ""
  | [last] -> (string_of_sexpr last)
  | head :: tail -> (string_of_sexpr head) ^ ", " ^ (string_of_sarray_primitive tail)

and string_of_sexpr = function 
    SIntLit(i) -> string_of_int i
  | SFloatLit(f) -> string_of_float f
  | SBoolLit(b) -> if b then "true" else "false"
  | SCharLit(c) -> Char.escaped c
  | SStringLit(s) -> "\"" ^ (String.escaped s) ^ "\""
  | SFunctionLit(s, data_t) ->
        s ^ ":" ^ string_of_datatype data_t
  | SId(s, _) -> s
  | SBinop(e1, o, e2, _) -> (string_of_sexpr e1) ^ " " ^ (string_of_op o) ^ " " ^ (string_of_sexpr e2)
  | SUnop(op, e, _) -> (string_of_uop op) ^ "(" ^ string_of_sexpr e ^ ")"
  | SAssign(e1, e2, _) -> (string_of_sexpr e1) ^ " = " ^ (string_of_sexpr e2) 
  | SObjAccess(e1, e2, data_t) -> 
          (string_of_sexpr e1) ^ "." ^ (string_of_sexpr e2) ^":"^ (string_of_datatype data_t)
  | SCall(ss, el, _, _) -> string_of_sexpr ss ^ "(" ^ String.concat ~sep:", " (List.map ~f:string_of_sexpr el) ^ ")"
  | SArrayAccess(se, se_l, _) ->
        string_of_sexpr se ^ "[" ^ string_of_bracket_sexpr se_l ^ "]"
  | SArrayCreate(d, se_l, _) -> 
        string_of_datatype d ^ "[" ^ string_of_bracket_sexpr se_l ^ "]"
  | SNoexpr -> ""
  | SThis(_) -> "this"

and string_of_local_sexpr = function
    SNoexpr -> ""
  | e -> " = " ^ string_of_sexpr e
 
and string_of_sstmt indent =
    let indent_string = String.make indent '\t' in
    let get_stmt_string = function 
        SBlock(stmts) -> 
            indent_string ^ "{\n" ^ 
            String.concat ~sep:"" (List.map ~f:(string_of_sstmt (indent+1)) stmts) ^ 
            indent_string ^ "}\n"
      | SExpr(expr, data_t) -> 
            indent_string ^ string_of_sexpr expr ^":" ^ string_of_datatype data_t ^";\n";
      | SReturn(expr, _)      -> 
            indent_string ^ "return " ^ string_of_sexpr expr ^ ";\n";
      | SIf(e, s, SBlock([SExpr(SNoexpr, _)]))  -> 
            indent_string ^ "if (" ^ string_of_sexpr e ^ ")\n" ^ 
            (string_of_sstmt (indent+1) s)
      | SIf(e, s1, s2) -> 
            indent_string ^ "if (" ^ string_of_sexpr e ^ ")\n" ^ 
            string_of_sstmt (indent+1) s1 ^ 
            indent_string ^ "else\n" ^ 
            string_of_sstmt (indent+1) s2
      | SFor(e1, e2, e3, s)     -> 
            indent_string ^ "for (" ^ string_of_sexpr e1  ^ " ; " ^ 
            string_of_sexpr e2 ^ " ; " ^ string_of_sexpr e3  ^ ")\n" ^ 
            string_of_sstmt (indent) s
      | SWhile(e, s) -> 
            indent_string ^ "while (" ^ string_of_sexpr e ^ ")\n" ^ 
            string_of_sstmt (indent) s
      | SBreak ->
            indent_string ^ "break;"
      | SContinue ->
            indent_string ^ "continue;"
      | SLocal(s, d, e) -> 
            indent_string ^ s ^ ":" ^ string_of_datatype d ^ 
            string_of_local_sexpr e ^ ";\n"
    in 
    get_stmt_string
  
and string_of_sfdecl sfdecl = 
    "function" ^ " " ^ sfdecl.sfname ^ " = (" ^
    String.concat ~sep:", " (List.map ~f:string_of_formal sfdecl.sformals) ^
    "):" ^ string_of_datatype sfdecl.sreturn_t ^ " {\n" ^ 
    string_of_sstmt 0 (SBlock(sfdecl.sbody)) ^
    "}\n"
      
and string_of_scdecl scdecl =
    "class " ^ scdecl.scname ^ " {\n" ^
    String.concat ~sep:"" (List.map ~f:string_of_field scdecl.sfields) ^
    String.concat ~sep:"" (List.map ~f:string_of_sfdecl scdecl.sfdecls) ^
    "}\n"
    
and string_of_main main = match main with 
    Some(sfdecl) -> string_of_sfdecl sfdecl
  | None -> ""

let string_of_sprogram sprogram =
    String.concat ~sep:"\n" (List.map ~f:string_of_scdecl sprogram.classes) ^ "\n" ^
    String.concat ~sep:"\n" (List.map ~f:string_of_sfdecl sprogram.functions) ^ "\n" ^
    string_of_sfdecl sprogram.main ^ "\n" 
    
