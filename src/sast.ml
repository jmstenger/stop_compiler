(* Semantically Checked AST *)
(* ------------------------ *)

(* Resolves datatypes in exprs, sstmt s *)

open Ast

type fgroup = User | Reserved

type sfdecl = {
    sfname : string;
    sreturn_t : datatype;
    srecord_vars : (string * datatype) list;
    sformals : formal list;
    sbody : sstmt list;
    fgroup : fgroup;
    overrides : bool;
    source : string option;
    sftype : datatype;
}

and scdecl = {
    scname : string;
    sfields : field list;
    sfdecls : sfdecl list;
}

and sprogram = {
    classes : scdecl list;
    functions : sfdecl list;
    main : sfdecl;
}

and sexpr = 
    SIntLit of int
  | SFloatLit of float
  | SBoolLit of bool
  | SCharLit of char
  | SStringLit of string
  | SFunctionLit of string * datatype
  | SId of string * datatype
  | SUnop of uop * sexpr * datatype
  | SBinop of sexpr * op * sexpr * datatype
  | SAssign of sexpr * sexpr * datatype
  | SCall of sexpr * sexpr list * datatype * int
  | SObjAccess of sexpr * sexpr * datatype
  | SArrayAccess of sexpr * sexpr list * datatype
  | SArrayCreate of datatype * sexpr list * datatype 
  | SThis of datatype
  | SNoexpr

and sstmt =
    SBlock of sstmt list
  | SExpr of sexpr * datatype
  | SReturn of sexpr * datatype
  | SIf of sexpr * sstmt * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt 
  | SWhile of sexpr * sstmt 
  | SLocal of string * datatype * sexpr
  | SBreak
  | SContinue
