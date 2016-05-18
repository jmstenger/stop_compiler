(* Stop Abstract Syntax Tree *)

type op = Add | Sub | Mult | Div | Modulo | And | Or |
          Equal | Neq | Less | Leq | Greater | Geq 
type uop = Neg | Not
type primitive = Int_t | Float_t | Bool_t | Char_t | Unit_t | Object_t of string
type scope = Private | Public
type extends = NoParent | Parent of string

(* Functions *)
(* --------- *)

type fdecl = {
    fname : string;
    ftype : datatype;
    return_t : datatype;
    formals : formal list;
    body : stmt list;
    scope : scope;
    overrides : bool;
    root_cname : string option;
}

(* Specs *)
(* ----- *)

and spec = {
    sname : string;
}

(* Classes *)
(* ------- *)

and cbody = {
    fields : field list;
    methods : fdecl list;
}

and cdecl = {
    cname : string;
    extends : extends;
    cbody: cbody;
}

(* Datatypes, Formals, & Fields *)
(* i.e. Arraytype (a, 2) <=> a[][]; (a, 3) <=> a[][][] *)

(* Any : used for type of functions that take any datatype e.g. Llvm cast *)
(* NoFunctiontype : used for type of LLVM Builtin C Functions *)
and datatype = 
    Datatype of primitive 
  | Arraytype of primitive * int
  | Functiontype of datatype list * datatype
  | NoFunctiontype
  | Any 

(* Many : used for type of variable length functions e.g. Llvm printf *)
and formal = Formal of string * datatype | Many of datatype

and field = Field of scope * string * datatype

(* Fields *)
(* ------ *)

and expr = 
    IntLit of int
  | FloatLit of float
  | BoolLit of bool
  | CharLit of char
  | StringLit of string
  | FunctionLit of fdecl
  | Id of string
  | Binop of expr * op * expr
  | Assign of expr * expr
  | Unop of uop * expr
  | Call of string * expr list
  | ArrayAccess of expr * expr list
  | ArrayCreate of datatype * expr list
  | ObjAccess of expr * expr
  | This
  | Noexpr

and stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | Local of string * datatype * expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | Break
  | Continue

and var = Var of datatype * string

and include_stmt = Include of string

(* Program Definition *)
(* ------------------ *)

type constituents = {
    includes : include_stmt list;
    specs : spec list;
    cdecls : cdecl list ;
    fdecls : fdecl list;
}

type program =  Program of include_stmt list * spec list * cdecl list * fdecl list

(*
type directive = Include of include_stmt
type constituent = Spec of spec | Class of cdecl | Function of fdecl
type program = Program of directive list * constituent list
*)
