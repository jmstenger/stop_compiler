(* Ocamllex scanner for Stop Language *)

{   
    open Core.Std
    open Parser 
    module E = Exceptions

    let lineno = ref 1
    let depth = ref 0
    let filename = ref ""

    let unescape s =
        Scanf.sscanf ("\"" ^ s ^ "\"") "%S%!" (fun x -> x)
}	

(* Helper Regexes *)
let whitespace = [' ' '\t' '\r']

let alpha = ['a'-'z' 'A'-'Z']
let upper_alpha = ['A'-'Z']
let lower_alpha = ['a'-'z']

let digit = ['0'-'9']
let exp = (('e'|'E')('-'|'+')?digit+)
let ascii = [' '-'!' '#'-'[' ']'-'~']
let escape_char = '\\' ['\\' ''' '"' 'n' 'r' 't']

(* Literals *) 
let int_lit = digit+ as lit
let float_lit = (digit+'.'digit*exp?)|(digit+'.'?digit*exp)
                    |(digit*'.'digit+exp?)|(digit*'.'?digit+exp) as lit
let char_lit = '''(ascii|digit as lit)'''
let escape_char_lit = '''(escape_char as lit)'''
let string_lit = '"'((ascii|escape_char)* as lit)'"'
let id = lower_alpha (alpha | digit | '_')* as lit
let typeid = upper_alpha (alpha | digit | '_')* as lit

rule token = parse
      whitespace    { token lexbuf }                   (* Whitespace *)
    | "//"      { single_comment lexbuf }           (* Comments *)
    | "/*"      { incr depth; multi_comment lexbuf }
    | '\n'      { incr lineno; token lexbuf }
	| '('       { LPAREN }
	| ')'       { RPAREN }
    | '{'       { LBRACE }
    | '}'       { RBRACE }
    | '['		{ LBRACKET }
    | ']'		{ RBRACKET }
    | ':'       { COLON }
    | ';'       { SEMI }
    | ','       { COMMA }
    | '.'       { DOT }

    (* Operators *)
	| "++"      { INCREMENT }
	| "--"      { DECREMENT }
	| '+'       { PLUS }
	| '-'       { MINUS }
	| '*'       { TIMES }
	| '/'       { DIVIDE }
	| '='       { ASSIGN }
	| '^'       { CARET }
    | '%'		{ MODULO }
    | "=="      { EQ }
    | "!="      { NEQ }
    | '<'       { LT }
    | "<="      { LEQ }
    | ">"       { GT }
    | ">="      { GEQ }
    | "&&"      { AND }
    | "||"      { OR }
    | "!"       { NOT }

    (* Misc *)
    | "->"              { ARROW }
    | "=>"              { FATARROW }
    | "public"          { PUBLIC }
    | "private"         { PRIVATE }
    | '@'               { ANON }

    (* Conditionals *)
    | "if"      { IF }
    | "else"    { ELSE }
    | "for"     { FOR }
    | "while"   { WHILE }
    | "break"   { BREAK }
    | "continue" { CONTINUE }
    | "return"  { RETURN }

    (* Reserved Keywords *)
    | "spec"        { SPEC } 
    | "class"	    { CLASS }
    | "method"      { METHOD }
    | "def"		    { DEF }
    | "var"         { VAR }
    | "type"        { TYPE }
    | "final"       { FINAL } 
    | "this"        { THIS }
    | "extends"     { EXTENDS }
    | "match"       { MATCH }
    | "case"        { CASE }

    (* Processor Directives *)
    | "#include"    { INCLUDE }
    | "#module"    { MODULE }

    (* TYPES *)
    | "Int"     { INT }
    | "Float"   { FLOAT }
    | "Bool"    { BOOL }
    | "Char"    { CHAR }
    | "Unit"    { UNIT }

    (* PRIMITIVE LITERALS *)
    | "true"    { TRUE }
    | "false"   { FALSE }
    | int_lit               { INT_LIT(int_of_string lit) }
    | float_lit             { FLOAT_LIT(float_of_string lit) }
    | char_lit              { CHAR_LIT(lit) }
    | escape_char_lit       { CHAR_LIT(String.get (unescape lit) 0) }
    | string_lit            { STRING_LIT(unescape lit) }
    | id                    { ID(lit) }
    | typeid                { TYPE_ID(lit) }
	| eof                   { EOF }
	| _ as illegal          { raise (E.IllegalCharacter(!filename, (Char.escaped illegal), !lineno)) }

and single_comment = parse
      '\n'      { incr lineno; token lexbuf }
    | _         { single_comment lexbuf }

and multi_comment = parse
      '\n'      { incr lineno; multi_comment lexbuf }
    | "/*"      { incr depth; multi_comment lexbuf }
    | "*/"      { decr depth; if !depth > 0 then multi_comment lexbuf  
                                            else token lexbuf }
    | _         { multi_comment lexbuf }
