(* Code Generation Phase *) 

(*
    Input: Semantically Checked AST (type sprogram)
    Output: LLVM Module 
       
    Produces an LLVM IR translation of the source program
    LLVM Tutorial:
        http://llvm.org/docs/tutorial/index.html
    LLVM Documentation:
        http://llvm.moe/
        http://llvm.moe/ocaml/
*)    

open Core.Std
open Sast 
open Ast

module A = Analysis
module E = Exceptions
module L = Llvm
module U = Utils
      
let context     = L.global_context ()
let the_module  = L.create_module context "Stop"
let builder     = L.builder context
let i32_t       = L.i32_type context
let i8_t        = L.i8_type context
let i1_t        = L.i1_type context 
let float_t     = L.float_type context
(*let double_t    = L.double_type context*)
let void_t      = L.void_type context 
let str_t       = L.pointer_type (L.i8_type context)

(* Control Flow References *)
let br_block    = ref (L.block_of_value (L.const_int i32_t 0))
let cont_block  = ref (L.block_of_value (L.const_int i32_t 0))
let is_loop     = ref false

let struct_types:(string, L.lltype) Hashtbl.t = Hashtbl.create ()
    ~hashable:String.hashable 
    ~size:10

let struct_field_indexes:(string, int) Hashtbl.t = Hashtbl.create ()
    ~hashable:String.hashable 
    ~size:50

(* Named Values inside current scope *)
let named_values:(string, L.llvalue) Hashtbl.t = Hashtbl.create ()
    ~hashable:String.hashable 
    ~size:50

(* Named parameters inside current scope *)
let named_parameters:(string, L.llvalue) Hashtbl.t = Hashtbl.create ()
    ~hashable:String.hashable 
    ~size:50

let str_type = Arraytype(Char_t, 1)

let rec get_array_type array_t = match array_t with
    Arraytype(prim, 1) -> L.pointer_type(get_lltype_exn (Datatype(prim)))
  | Arraytype(prim, i) -> L.pointer_type(get_array_type (Arraytype(prim, i-1)))
  | _ -> raise(E.InvalidDatatype "Array Type")

and find_struct_exn name =
    if name = "String" then (L.i8_type context) else 
    try
        Hashtbl.find_exn struct_types name
    with
        Not_found -> raise (E.InvalidStructType(name))

and get_function_type data_t_list return_t =
    let llargs = List.fold_left (List.rev data_t_list)
        ~f:(fun l data_t -> get_lltype_exn data_t :: l)
        ~init:[]
    in
    L.pointer_type (L.function_type (get_lltype_exn return_t) (Array.of_list llargs))

and get_lltype_exn (data_t:datatype) = match data_t with
    Datatype(Int_t) -> i32_t
  | Datatype(Float_t) -> float_t (* TODO: Decide what to do a/b doubles & floats *)
  | Datatype(Bool_t) -> i1_t
  | Datatype(Char_t) -> i8_t
  | Datatype(Unit_t) -> void_t
  | Datatype(Object_t(name)) -> L.pointer_type(find_struct_exn name)
  | Arraytype(t, i) -> get_array_type (Arraytype(t, i))
  | Functiontype(dt_l, dt) -> get_function_type dt_l dt
  | data_t -> raise (E.InvalidDatatype(U.string_of_datatype data_t))

let lookup_llfunction_exn fname = match (L.lookup_function fname the_module) with
    None -> raise (E.LLVMFunctionNotFound(fname))
  | Some f -> f

let rec codegen_sexpr sexpr ~builder:llbuilder = match sexpr with 
    SIntLit(i)                  -> L.const_int i32_t i
  | SFloatLit(f)                -> L.const_float float_t f
  | SBoolLit(b)                 -> if b then L.const_int i1_t 1 else L.const_int i1_t 0
  | SCharLit(c)                 -> L.const_int i8_t (Char.to_int c)
  | SStringLit(s)               -> L.build_global_stringptr s "tmp" llbuilder
  | SFunctionLit(s, _)          -> codegen_function_lit s llbuilder
  | SAssign(e1, e2, _)          -> codegen_assign e1 e2 llbuilder
  | SArrayAccess(se, se_l, _)   -> codegen_array_access false se se_l llbuilder
  | SObjAccess(se1, se2, d)     -> codegen_obj_access true se1 se2 d llbuilder
  | SNoexpr                     -> L.build_add (L.const_int i32_t 0) (L.const_int i32_t 0) "nop" llbuilder
  | SId(id, _)                      -> codegen_id false id llbuilder
  | SBinop(e1, op, e2, data_t)      -> handle_binop e1 op e2 data_t llbuilder
  | SUnop(op, e, d)                 -> handle_unop op e d llbuilder
  | SCall(fname, se_l, data_t, _)   -> codegen_call fname se_l data_t llbuilder
  | SArrayCreate(t, el, d)          -> codegen_array_create llbuilder t d el 
  | _ -> raise E.NotImplemented
 (* | SObjectCreate(id, el, d)    -> codegen_obj_create id el d llbuilder *)
 (* | SArrayPrimitive(el, d)      -> codegen_array_prim d el llbuilder
  | SNull                       -> const_null i32_t
  | SDelete e                   -> codegen_delete e llbuilder
    *)

(* Generate Code for Binop *)
and handle_binop e1 op e2 data_t llbuilder = 
    (* Get the types of e1 and e2 *)
    let type1 = A.sexpr_to_type e1 in
    let type2 = A.sexpr_to_type e2 in

    (* Generate llvalues from e1 and e2 *)
    let e1 = codegen_sexpr e1 ~builder:llbuilder in
    let e2 = codegen_sexpr e2 ~builder:llbuilder in

    (* Integer Llvm functions *)
    let int_ops e1 op e2 =
        match op with
            Add     -> L.build_add e1 e2 "addtmp" llbuilder
          | Sub     -> L.build_sub e1 e2 "subtmp" llbuilder
          | Mult    -> L.build_mul e1 e2 "multmp" llbuilder
          | Div     -> L.build_sdiv e1 e2 "divtmp" llbuilder
          | Modulo  -> L.build_srem e1 e2 "sremtmp" llbuilder
          | Equal   -> L.build_icmp L.Icmp.Eq e1 e2 "eqtmp" llbuilder
          | Neq     -> L.build_icmp L.Icmp.Ne e1 e2 "neqtmp" llbuilder
          | Less    -> L.build_icmp L.Icmp.Slt e1 e2 "lesstmp" llbuilder
          | Leq     -> L.build_icmp L.Icmp.Sle e1 e2 "leqtmp" llbuilder
          | Greater -> L.build_icmp L.Icmp.Sgt e1 e2 "sgttmp" llbuilder
          | Geq     -> L.build_icmp L.Icmp.Sge e1 e2 "sgetmp" llbuilder
          | And     -> L.build_and e1 e2 "andtmp" llbuilder
          | Or      -> L.build_or  e1 e2 "ortmp" llbuilder
          | _       -> raise Exceptions.IntOpNotSupported
    in

    (* Floating Point Llvm functions *)
    let float_ops e1 op e2 =
        match op with
            Add     -> L.build_fadd e1 e2 "flt_addtmp" llbuilder
          | Sub     -> L.build_fsub e1 e2 "flt_subtmp" llbuilder
          | Mult    -> L.build_fmul e1 e2 "flt_multmp" llbuilder
          | Div     -> L.build_fdiv e1 e2 "flt_divtmp" llbuilder
          | Modulo  -> L.build_frem e1 e2 "flt_sremtmp" llbuilder
          | Equal   -> L.build_fcmp L.Fcmp.Oeq e1 e2 "flt_eqtmp" llbuilder
          | Neq     -> L.build_fcmp L.Fcmp.One e1 e2 "flt_neqtmp" llbuilder
          | Less    -> L.build_fcmp L.Fcmp.Ult e1 e2 "flt_lesstmp" llbuilder
          | Leq     -> L.build_fcmp L.Fcmp.Ole e1 e2 "flt_leqtmp" llbuilder
          | Greater -> L.build_fcmp L.Fcmp.Ogt e1 e2 "flt_sgttmp" llbuilder
          | Geq     -> L.build_fcmp L.Fcmp.Oge e1 e2 "flt_sgetmp" llbuilder
          | _       -> raise Exceptions.FloatOpNotSupported
    in

    (* Use Integer Arithmetic for Ints, Chars, and Bools *)
    (* Use Floating-Point Arithmetic for Floats *)
    let type_handler data_t = match data_t with
        Datatype(Int_t) 
      | Datatype(Char_t)
      | Datatype(Bool_t) -> int_ops e1 op e2
      | Datatype(Float_t) -> float_ops e1 op e2
      | _ -> raise E.InvalidBinopEvaluationType
    in
    type_handler data_t

and handle_unop op se data_t llbuilder =
    let se_type = A.sexpr_to_type_exn se in
    let llvalue = codegen_sexpr se llbuilder in

    let unops op se_type llval = match (op, se_type) with
        (Neg, Datatype(Int_t))      -> L.build_neg llvalue "int_unoptmp" llbuilder
      | (Neg, Datatype(Float_t))    -> L.build_fneg llvalue "flt_unoptmp" llbuilder
      | (Not, Datatype(Bool_t))     -> L.build_not llvalue "bool_unoptmp" llbuilder
      | _ -> raise E.UnopNotSupported
    in

    let type_handler data_t = match data_t with
        Datatype(Float_t)
      | Datatype(Int_t)
      | Datatype(Bool_t) -> unops op se_type llvalue
      | _ -> raise E.InvalidUnopEvaluationType
    in

    type_handler data_t

and codegen_call sexpr sexpr_l data_t llbuilder = match sexpr with
    SId(fname, _) -> 
        (match fname with
            "printf" -> codegen_printf sexpr_l llbuilder
          | _ -> codegen_function_call sexpr sexpr_l data_t llbuilder)
  | _ -> codegen_function_call sexpr sexpr_l data_t llbuilder

and codegen_function_call sexpr sexpr_l data_t llbuilder =
    let call_function fllval =
        let params = List.map ~f:(codegen_sexpr ~builder:llbuilder) sexpr_l in
        match data_t with
            Datatype(Unit_t) -> L.build_call fllval (Array.of_list params) "" llbuilder
          | _ -> L.build_call fllval (Array.of_list params) "tmp" llbuilder
    in
    match sexpr with
        SId(fname, _) -> 
            let f = lookup_llfunction_exn fname in 
            call_function f
      | SObjAccess(se1, se2, data_t) -> 
            let f = codegen_obj_access true se1 se2 data_t llbuilder in
            call_function f

and codegen_printf sexpr_l llbuilder =
    (* Convert printf format string to llvalue *)
    let format_str = List.hd_exn sexpr_l in
    let format_llstr = match format_str with
        SStringLit(s) -> L.build_global_stringptr s "fmt" llbuilder
      | _ -> raise E.PrintfFirstArgNotString
    in
    (* Convert printf args to llvalue *)
    let args = List.tl_exn sexpr_l in
    let format_llargs = List.map args ~f:(codegen_sexpr ~builder:llbuilder) in
    (* Build printf call *)
    let fun_llvalue = lookup_llfunction_exn "printf" in
    let llargs = Array.of_list (format_llstr :: format_llargs) in
    L.build_call fun_llvalue llargs "printf" llbuilder

and codegen_id isDeref id llbuilder = 
    if isDeref then
        try Hashtbl.find_exn named_parameters id
        with | Not_found ->
            try let var = Hashtbl.find_exn named_values id in
                L.build_load var id llbuilder 
            with | Not_found -> raise (E.UndefinedId id)
    else
        try Hashtbl.find_exn named_parameters id
        with | Not_found ->
            try Hashtbl.find_exn named_values id 
            with | Not_found -> raise (E.UndefinedId id)

and codegen_assign se1 se2 llbuilder =
    (* Get lhs llvalue; don't emit as expression *)
    let lhs = match se1 with
        SId(id, _) -> 
            (try Hashtbl.find_exn named_parameters id
            with Not_found ->
                try Hashtbl.find_exn named_values id
                with Not_found -> raise (E.UndefinedId id))
      | SObjAccess(se1, se2, data_t) -> codegen_obj_access false se1 se2 data_t llbuilder
      | SArrayAccess(se, se_l, _) -> 
            codegen_array_access true se se_l llbuilder
      | _ -> raise E.AssignmentLhsMustBeAssignable
    in
    (* Get rhs llvalue *)
    let rhs = match se2 with 
        SObjAccess(se1, se2, data_t) -> codegen_obj_access true se1 se2 data_t llbuilder
      | _ -> codegen_sexpr se2 ~builder:llbuilder 
    in
    (* Codegen Assignment Stmt *)
    ignore(L.build_store rhs lhs llbuilder);
    rhs

and codegen_obj_access isAssign lhs rhs data_t llbuilder =
    let obj_type_name = match lhs with
        SId(_, data_t) -> U.string_of_datatype data_t
      | SObjAccess(_, _, data_t) -> U.string_of_datatype data_t

    in 
    let struct_llval = match lhs with
        SId(s, _) -> codegen_id false s llbuilder
      | SObjAccess(le, re, data_t) -> codegen_obj_access true le re data_t llbuilder
    in
    let field_name = match rhs with
        SId(field, _) -> field
    in
    let field_type = match rhs with
        SId(_, data_t) -> data_t
    in
    let search_term = obj_type_name ^ "." ^ field_name in
    let field_index = Hashtbl.find_exn struct_field_indexes search_term in
    let llvalue = L.build_struct_gep struct_llval field_index field_name llbuilder in
    let llvalue = if isAssign 
        then L.build_load llvalue field_name llbuilder
        else llvalue
    in
    llvalue

and codegen_array_access isAssign e e_l llbuilder =
    let indices = List.map e_l ~f:(codegen_sexpr ~builder:llbuilder) in
    let indices = Array.of_list indices in
    let arr = codegen_sexpr e ~builder:llbuilder in
    let llvalue =L.build_gep arr indices "tmp" llbuilder in
    if isAssign
        then llvalue
        else L.build_load llvalue "tmp" llbuilder

and codegen_function_lit fname llbuilder =
    let f_llval = lookup_llfunction_exn fname in
    f_llval

and codegen_return sexpr llbuilder = match sexpr with
    SNoexpr -> L.build_ret_void llbuilder
  | _ -> L.build_ret (codegen_sexpr sexpr ~builder:llbuilder) llbuilder

and codegen_break llbuilder = 
    let b = fun () -> !br_block in
    L.build_br (b ()) llbuilder

and codegen_continue llbuilder = 
    let b = fun () -> !cont_block in
    L.build_br (b ()) llbuilder
    
(* TODO: Alloca vs. Malloc *)
and codegen_local var_name data_t sexpr llbuilder = 
    let lltype = match data_t with
        Datatype(Object_t(name)) -> find_struct_exn name
      | _ -> get_lltype_exn data_t
    in
    (* let alloca = L.build_alloca lltype var_name llbuilder in *)
    let malloc = L.build_malloc lltype var_name llbuilder in

    Hashtbl.add_exn named_values ~key:var_name ~data:malloc;
    let lhs = SId(var_name, data_t) in
    match sexpr with
        SNoexpr -> malloc
      | _ -> codegen_assign lhs sexpr llbuilder

and codegen_stmt stmt ~builder:llbuilder = match stmt with
    SBlock(sl)              -> List.hd_exn (List.map ~f:(codegen_stmt ~builder:llbuilder) sl)
  | SExpr(se, _)            -> codegen_sexpr se llbuilder
  | SReturn(se, _)          -> codegen_return se llbuilder
  | SLocal(s, data_t, se)   -> codegen_local s data_t se llbuilder
  | SIf(se, s1, s2)         -> codegen_if_stmt se s1 s2 llbuilder 
  | SFor(se1, se2, se3, ss) -> codegen_for_stmt se1 se2 se3 ss llbuilder
  | SWhile(se, ss)          -> codegen_while_stmt se ss llbuilder
  | SBreak                  -> codegen_break llbuilder
  | SContinue               -> codegen_continue llbuilder

and codegen_if_stmt predicate then_stmt else_stmt llbuilder =
    let cond_val = codegen_sexpr predicate llbuilder in
    let start_bb = L.insertion_block llbuilder in
    let the_function = L.block_parent start_bb in

    let then_bb = L.append_block context "then" the_function in

    L.position_at_end then_bb llbuilder;
    let _ = codegen_stmt then_stmt llbuilder in

    let new_then_bb = L.insertion_block llbuilder in

    let else_bb = L.append_block context "else" the_function in
    L.position_at_end else_bb llbuilder;
    let _ = codegen_stmt else_stmt llbuilder in

    let new_else_bb = L.insertion_block llbuilder in
    let merge_bb = L.append_block context "ifcont" the_function in
    L.position_at_end merge_bb llbuilder;

    let else_bb_val = L.value_of_block new_else_bb in
    L.position_at_end start_bb llbuilder;

    ignore (L.build_cond_br cond_val then_bb else_bb llbuilder);
    L.position_at_end new_then_bb llbuilder; ignore (L.build_br merge_bb llbuilder);
    L.position_at_end new_else_bb llbuilder; ignore (L.build_br merge_bb llbuilder);
    L.position_at_end merge_bb llbuilder;
    else_bb_val

and codegen_for_stmt init_se cond_se inc_se body_stmt llbuilder =
    let old_val = !is_loop in
    is_loop := true;

    let the_function = L.block_parent (L.insertion_block llbuilder) in
    let _ = codegen_sexpr init_se llbuilder in

    let loop_bb = L.append_block context "loop" the_function in
    let inc_bb = L.append_block context "inc" the_function in
    let cond_bb = L.append_block context "cond" the_function in
    let after_bb = L.append_block context "afterloop" the_function in

    let _ = if not old_val then
        cont_block := inc_bb;
        br_block := after_bb;
    in
    ignore (L.build_br cond_bb llbuilder);

    (* Start insertion in loop_bb. *)
    L.position_at_end loop_bb llbuilder;

    (* Emit the body of the loop.  This, like any other expr, can change the
    * current BB.  Note that we ignore the value computed by the body, but
    * don't allow an error *)
    ignore (codegen_stmt body_stmt ~builder:llbuilder);

    let bb = L.insertion_block llbuilder in
    L.move_block_after bb inc_bb;
    L.move_block_after inc_bb cond_bb;
    L.move_block_after cond_bb after_bb;
    ignore(L.build_br inc_bb llbuilder);

    (* Start insertion in loop_bb. *)
    L.position_at_end inc_bb llbuilder;
 
    (* Emit the step value. *)
    let _ = codegen_sexpr inc_se llbuilder in
    ignore(L.build_br cond_bb llbuilder);

    L.position_at_end cond_bb llbuilder;

    let cond_val = codegen_sexpr cond_se llbuilder in
    ignore (L.build_cond_br cond_val loop_bb after_bb llbuilder);
    L.position_at_end after_bb llbuilder;
    is_loop := old_val;
    L.const_null float_t

and codegen_while_stmt cond_se body_stmt llbuilder =
    let null_sexpr = SIntLit(0) in
    codegen_for_stmt null_sexpr cond_se null_sexpr body_stmt llbuilder

and codegen_array_create llbuilder t expr_type el = 
  if(List.length el > 1) then raise(Exceptions.ArrayLargerThan1Unsupported)
  else
  match expr_type with 
    Arraytype(Char_t, 1) -> 
    let e = List.hd_exn el in
    let size = (codegen_sexpr e llbuilder) in
    let t = get_lltype_exn t in
    let arr = L.build_array_malloc t size "tmp" llbuilder in
    let arr = L.build_pointercast arr (L.pointer_type t) "tmp" llbuilder in
    (* initialise_array arr size (const_int i32_t 0) 0 llbuilder; *)
    arr
  |   _ -> 
    let e = List.hd_exn el in
    let t = get_lltype_exn t in

    (* This will not work for arrays of objects *)
    let size = (codegen_sexpr e llbuilder) in
    let size_t = L.build_intcast (L.size_of t) i32_t "tmp" llbuilder in
    let size = L.build_mul size_t size "tmp" llbuilder in
    let size_real = L.build_add size (L.const_int i32_t 1) "arr_size" llbuilder in
    
      let arr = L.build_array_malloc t size_real "tmp" llbuilder in
    let arr = L.build_pointercast arr (L.pointer_type t) "tmp" llbuilder in

    let arr_len_ptr = L.build_pointercast arr (L.pointer_type i32_t) "tmp" llbuilder in

    (* Store length at this position *)
    ignore(L.build_store size_real arr_len_ptr llbuilder); 
    (* initialise_array arr_len_ptr size_real (const_int i32_t 0) 0 llbuilder; *)
    arr

(* Codegen Library Functions *)
(* ========================= *)

let codegen_library_functions () = 
    (* C Std lib functions (Free with Llvm) *)
    let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
    let _ = L.declare_function "printf" printf_t the_module in
    let malloc_t = L.function_type (str_t) [| i32_t |] in
    let _ = L.declare_function "malloc" malloc_t the_module in
    let open_t = L.function_type i32_t [| (L.pointer_type i8_t); i32_t |] in 
    let _ = L.declare_function "open" open_t the_module in
    let close_t = L.function_type i32_t [| i32_t |] in
    let _ = L.declare_function "close" close_t the_module in
    let read_t = L.function_type i32_t [| i32_t; L.pointer_type i8_t; i32_t |] in
    let _ = L.declare_function "read" read_t the_module in
    let write_t = L.function_type i32_t [| i32_t; L.pointer_type i8_t; i32_t |] in
    let _ = L.declare_function "write" write_t the_module in 
    let lseek_t = L.function_type i32_t [| i32_t; i32_t; i32_t |] in
    let _ = L.declare_function "lseek" lseek_t the_module in
    let exit_t = L.function_type void_t [| i32_t |] in
    let _ = L.declare_function "exit" exit_t the_module in
    let realloc_t = L.function_type str_t [| str_t; i32_t |] in
    let _ = L.declare_function "realloc" realloc_t the_module in
    let getchar_t = L.function_type (i32_t) [| |] in
    let _ = L.declare_function "getchar" getchar_t the_module in
    let sizeof_t = L.function_type (i32_t) [| i32_t |] in
    let _ = L.declare_function "sizeof" sizeof_t the_module in 
    ()

let codegen_struct_stub s =
    let struct_t = L.named_struct_type context s.scname
    in
    Hashtbl.add struct_types
        ~key:s.scname
        ~data:struct_t

let codegen_struct s =
    let struct_t = Hashtbl.find_exn struct_types s.scname in
    let type_list = List.map s.sfields 
        ~f:(function Field(_, _, data_t) -> get_lltype_exn data_t)
    in
    let name_list = List.map s.sfields
        ~f:(function Field(_, s, _) -> s)  
    in

    (* Add key field to all structs *)
    let type_list = i32_t :: type_list in
    let name_list = ".key" :: name_list in
    let type_array = Array.of_list type_list in
    List.iteri name_list
        ~f:(fun i f -> 
            let n = s.scname ^ "." ^ f in
            (* print_string (n ^ "\n"); *)
            Hashtbl.add_exn struct_field_indexes ~key:n ~data:i);
    (* Add the struct to the module *)
    L.struct_set_body struct_t type_array true

let codegen_function_stub sfdecl =
    let fname = sfdecl.sfname in
    let is_var_arg = ref false in
    let params = List.rev 
        (List.fold_left sfdecl.sformals
            ~f:(fun l -> (function 
                Formal(_, data_t) -> get_lltype_exn data_t :: l
              | _ -> is_var_arg := true; l))
            ~init: [])
    in
    let ftype = 
        if !is_var_arg
        then L.var_arg_function_type (get_lltype_exn sfdecl.sreturn_t) (Array.of_list params)
        else L.function_type (get_lltype_exn sfdecl.sreturn_t) (Array.of_list params)
    in
    L.define_function fname ftype the_module

let init_params f formals =
    let formals = Array.of_list formals in
    Array.iteri (L.params f)
        ~f:(fun i element ->
            let n = formals.(i) in
            let n = U.string_of_formal_name n in
            L.set_value_name n element;
            Hashtbl.add_exn named_parameters
                ~key:n
                ~data:element;
            )

let codegen_function sfdecl =
    Hashtbl.clear named_values;
    Hashtbl.clear named_parameters;
    let fname = sfdecl.sfname in
    let f = lookup_llfunction_exn fname in
    let llbuilder = L.builder_at_end context (L.entry_block f) in

    let _ = init_params f sfdecl.sformals in
    let _ = codegen_stmt (SBlock(sfdecl.sbody)) ~builder:llbuilder in
    
    (* Check to make sure we return; add a return statement if not *)
    let last_bb = match (L.block_end (lookup_llfunction_exn fname)) with
        L.After(block) -> block
      | L.At_start(_) -> raise (E.FunctionWithoutBasicBlock(fname))
    in

    (* TODO: Return this return type (not working for some reason) *)
    let return_t = L.return_type (L.type_of (lookup_llfunction_exn fname)) in
    match (L.instr_end last_bb) with
        L.After(instr) ->
            let op = L.instr_opcode instr in
            if op = L.Opcode.Ret 
            then ()
            else 
                if return_t = void_t
                then (ignore(L.build_ret_void); ())
                else (ignore(L.build_ret (L.const_int i32_t 0) llbuilder); ())
      | L.At_start(_) -> 
            if return_t = void_t
            then (ignore(L.build_ret_void); ())
            else (ignore(L.build_ret (L.const_int i32_t 0) llbuilder); ())

let codegen_main main =
    Hashtbl.clear named_values;
    Hashtbl.clear named_parameters;
    let ftype = L.function_type i32_t [| i32_t; L.pointer_type str_t |] in 
    let f = L.define_function "main" ftype the_module in
    let llbuilder = L.builder_at_end context (L.entry_block f) in

    let argc = L.param f 0 in
    let argv = L.param f 1 in
    L.set_value_name "argc" argc;
    L.set_value_name "argv" argv;
    Hashtbl.add_exn named_parameters ~key:"argc" ~data:argc;
    Hashtbl.add_exn named_parameters ~key:"argv" ~data:argv;

    let _ = codegen_stmt (SBlock(main.sbody)) llbuilder in

    (* Check to make sure we return; add a return statement if not *)
    let last_bb = match (L.block_end (lookup_llfunction_exn "main")) with
        L.After(block) -> block
      | L.At_start(_) -> raise (E.FunctionWithoutBasicBlock("main"))
    in
    match (L.instr_end last_bb) with
        L.After(instr) ->
            let op = L.instr_opcode instr in
            if op = L.Opcode.Ret 
            then ()
            else ignore(L.build_ret (L.const_int i32_t 0) llbuilder); ()
      | L.At_start(_) -> ignore(L.build_ret (L.const_int i32_t 0) llbuilder); ()

let codegen_sast sast =
    (* Declare the various LLVM Reserved Functions *)
    let _ = codegen_library_functions () in
    (* Generate a map of class names to their respective LLVM Struct Types *)
    let _ = List.map sast.classes ~f:(fun s -> codegen_struct_stub s) in
    (* Generate LLVM IR for classes *)
    let _ = List.map sast.classes ~f:(fun s -> codegen_struct s) in
    (* Define the program functions *)
    let _ = List.map sast.functions ~f:(fun f -> codegen_function_stub f) in
    (* Generate LLVM IR for functions *)
    let _ = List.map sast.functions ~f:(fun f -> codegen_function f) in
    (* Generate LLVM IR for main function *)
    let _ = codegen_main sast.main in
    the_module
