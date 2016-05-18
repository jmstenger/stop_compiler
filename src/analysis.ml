(* Semantic Analyzer for Stop Language *)

open Core.Std
open Ast
open Sast

module E = Exceptions
module G = Generator
module U = Utils

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

let seed_index = ref 0;;

(* General String of List Function *)
let string_of_list string_of_item l =
    "[" ^ String.concat ~sep:", " (List.map ~f:string_of_item l) ^ "]"

let higher_order_sfdecls = ref StringMap.empty

(* Type of access link to pass to function *)
let access_link_types:(string, datatype) Hashtbl.t = Hashtbl.create ()
    ~hashable:String.hashable
    ~size:10

let access_link_fnames:(string, string) Hashtbl.t = Hashtbl.create ()
    ~hashable:String.hashable
    ~size:10

(* Record which contains information re: Classes *)
type class_record = {
    field_map       : field StringMap.t;
    method_map      : fdecl StringMap.t;
    cdecl           : cdecl;
    (* constructor_map : Ast.fdecl StringMap.t; *)
}

(* Analysis Environment *)
(* Named vars = vars in scope *)
(* Record vars = vars to be placed in function activation record *)
type env = {
    env_cname               : string option;
    env_crecord             : class_record option;
    env_cmap                : class_record StringMap.t;
    env_fname               : string option;
    env_fmap                : fdecl StringMap.t;
    env_named_vars          : datatype StringMap.t;
    env_record_vars         : datatype StringMap.t;
    env_record_to_pass      : (string * datatype) StringMap.t;
    env_return_t            : datatype;
    env_in_for              : bool;
    env_in_while            : bool;
}

let update_env_cname env_cname env =
{
    env_cname           = env_cname;
    env_crecord         = env.env_crecord;
    env_cmap            = env.env_cmap;
    env_fname           = env.env_fname;
    env_fmap            = env.env_fmap;
    env_named_vars      = env.env_named_vars;
    env_record_vars     = env.env_record_vars;
    env_record_to_pass  = env.env_record_to_pass;
    env_return_t        = env.env_return_t;
    env_in_for          = env.env_in_for;
    env_in_while        = env.env_in_while;
}

let update_call_stack in_for in_while env =
{
    env_cname           = env.env_cname;
    env_crecord         = env.env_crecord;
    env_cmap            = env.env_cmap;
    env_fname           = env.env_fname;
    env_fmap            = env.env_fmap;
    env_named_vars      = env.env_named_vars;
    env_record_vars     = env.env_record_vars;
    env_record_to_pass  = env.env_record_to_pass;
    env_return_t        = env.env_return_t;
    env_in_for          = in_for;
    env_in_while        = in_while;
}

let get_fname_exn fname_option = match fname_option with
    Some(s) -> s
  | None -> raise E.UnexpectedNoFname

(* Name all methods <cname>.<fname> *)
let get_method_name cname fdecl =
    let name = fdecl.fname in
    cname ^ "." ^ name

let build_reserved_map =
    (* Note: ftype for printf has no functional equivalent *)
    let reserved_stub fname return_t formals =
        {
            sfname          = fname;
            sreturn_t       = return_t;
            sformals        = formals;
            srecord_vars    = [];
            sbody           = [];
            fgroup          = Sast.Reserved;
            overrides       = false;
            source          = None;
            sftype          = NoFunctiontype;
        }
    in
    let i32_t = Datatype(Int_t) in
    let void_t = Datatype(Unit_t) in
    let str_t = Arraytype(Char_t, 1) in
    let f s data_t = Formal(s, data_t) in
    let reserved_list = [
        reserved_stub "printf" void_t [Many(Any)];
        reserved_stub "malloc" str_t  [f "size" i32_t ];
        reserved_stub "cast" Any [f "in" Any];
        reserved_stub "sizeof" i32_t [f "in" Any];
        reserved_stub "open" i32_t [f "path" str_t; f "flags" i32_t];
        reserved_stub "close" i32_t [f "fd" i32_t];
        reserved_stub "read" i32_t [f "fd" i32_t; f "buf" str_t; f "nbyte" i32_t];
        reserved_stub "write" i32_t [f "fd" i32_t; f "buf" str_t; f "nbyte" i32_t];
        reserved_stub "lseek" i32_t [f "fd" i32_t; f "offset" i32_t; f "whence" i32_t];
        reserved_stub "exit" (void_t) ([f "status" i32_t]);
        reserved_stub "getchar" (i32_t) ([]);
        reserved_stub "input" (str_t) ([]);
    ]
    in
    let reserved_map =
        List.fold_left reserved_list
            ~init:StringMap.empty
            ~f:(fun m f -> StringMap.add m ~key:f.sfname ~data:f)
    in
    reserved_map

let rec expr_to_sexpr e env = match e with
    (* Literals *)
    IntLit(i)           -> (SIntLit(i), env)
  | FloatLit(b)         -> (SFloatLit(b), env)
  | BoolLit(b)          -> (SBoolLit(b), env)
  | CharLit(c)          -> (SCharLit(c), env)
  | StringLit(s)        -> (SStringLit(s), env)
  | Id(s)               -> (check_record_access s env, env)
  (*
  | Id(s)               -> (SId(s, get_Id_type s env), env)
  | This                -> (SId("this", get_this_type env), env)
    *)
  | Noexpr              -> (SNoexpr, env)

    (* Operations *)
  | Unop(op, e)         -> (check_unop op e env, env)
  | Binop(e1, op, e2)   -> (check_binop e1 op e2 env, env)
  | Assign(e1, e2)      -> (check_assign e1 e2 env, env)
  | Call(s, e_l)        -> (check_call s e_l env, env)
  | ArrayAccess(e, e_l) -> (check_array_access e e_l env, env)
  | ArrayCreate(d, e_l) -> (check_array_create d e_l env, env)
  | FunctionLit(f)      -> (check_function_literal f env, env)
  | ObjAccess(e1, e2)   -> (check_obj_access e1 e2 env, env)

(* Return Datatype for Binops with an Equality Operator (=, !=) *)
and get_equality_binop_type se1 op se2 =
    let type1 = sexpr_to_type_exn se1 in
    let type2 = sexpr_to_type_exn se2 in
    match (type1, type2) with
        (Datatype(Char_t), Datatype(Int_t))
      | (Datatype(Int_t), Datatype(Char_t)) ->
              SBinop(se1, op, se2, Datatype(Bool_t))
      | _ ->
              if type1 = type2
              then SBinop(se1, op, se2, Datatype(Bool_t))
              else
                  let type1 = U.string_of_datatype type1 in
                  let type2 = U.string_of_datatype type2 in
                  raise (E.InvalidEqualityBinop(type1, type2))

(* Return Datatype for Binops with a Logical Operator (&&, ||) *)
and get_logical_binop_type se1 op se2 =
    let type1 = sexpr_to_type_exn se1 in
    let type2 = sexpr_to_type_exn se2 in
    let operable = Set.of_list [Datatype(Int_t); Datatype(Char_t); Datatype(Bool_t)]
        ~comparator: Comparator.Poly.comparator
    in
    if Set.mem operable type1 && Set.mem operable type2
    then SBinop(se1, op, se2, Datatype(Bool_t))
    else raise E.InvalidBinaryOperation

(* Return Datatype for Binops with a Comparison Operator (<, <=, >, >=) *)
and get_comparison_binop_type se1 op se2 =
    let type1 = sexpr_to_type_exn se1 in
    let type2 = sexpr_to_type_exn se2 in
    let numerics = Set.of_list [Datatype(Int_t); Datatype(Float_t); Datatype(Char_t)]
        ~comparator: Comparator.Poly.comparator
    in
    if Set.mem numerics type1 && Set.mem numerics type2
    then SBinop(se1, op, se2, Datatype(Bool_t))
    else raise E.InvalidBinaryOperation

(* TODO: Handle casting *)

(* Return Datatype for Binops with an Arithemetic Operator (+, *, -, /, %) *)
and get_arithmetic_binop_type se1 op se2 =
    let type1 = sexpr_to_type_exn se1 in
    let type2 = sexpr_to_type_exn se2 in
    match (type1, type2) with
        (Datatype(Int_t), Datatype(Int_t))  -> SBinop(se1, op, se2, Datatype(Int_t))
      | (Datatype(Float_t), Datatype (Float_t)) -> SBinop(se1, op, se2, Datatype(Float_t))
      | _ -> raise E.InvalidBinaryOperation

(* Return Datatype for ID *)
and get_Id_type s env =
    try StringMap.find_exn env.env_named_vars s
    with | Not_found ->
        (*
        StringMap.iter env.env_named_vars
            ~f:(fun ~key:k ~data:data -> print_string (k ^ "\n"));
            *)
        raise (E.UndefinedId s)

and get_this_type env = match env.env_cname with
    Some(cname) -> Datatype(Object_t(cname))
  | None -> raise E.ThisUsedOutsideClass

and check_unop op e env =
    let check_num_unop op data_t = match op with
        Neg -> data_t
      | _ -> raise E.InvalidUnaryOperation
    in
    let check_bool_unop op = match op with
        Not -> Datatype(Bool_t)
      | _ -> raise E.InvalidUnaryOperation
    in
    let (se, env) = expr_to_sexpr e env in
    let data_t = sexpr_to_type_exn se in
    match data_t with
        Datatype(Int_t)
      | Datatype(Float_t)
      | Datatype(Char_t) -> SUnop(op, se, check_num_unop op data_t)
      | Datatype(Bool_t) -> SUnop(op, se, check_bool_unop op)
      | _ -> raise E.InvalidUnaryOperation

and check_binop e1 op e2 env =
    (* NOTE: may want to keep returned env *)
    let (se1, _) = expr_to_sexpr e1 env in
    let (se2, _) = expr_to_sexpr e2 env in
    match op with
        Equal
      | Neq -> get_equality_binop_type se1 op se2
      | And
      | Or -> get_logical_binop_type se1 op se2
      | Less
      | Leq
      | Greater
      | Geq -> get_comparison_binop_type se1 op se2
      | Add
      | Mult
      | Sub
      | Div
      | Modulo -> get_arithmetic_binop_type se1 op se2
      | _ -> raise E.InvalidBinaryOperation

and check_assign e1 e2 env =
    (* NOTE: may want to keep returned env *)
    let (se1, _) = expr_to_sexpr e1 env in
    let (se2, _) = expr_to_sexpr e2 env in
    let type1 = sexpr_to_type_exn se1 in
    let type2 = sexpr_to_type_exn se2 in
    match (type1, type2) with
        _ -> if type1 = type2
            then SAssign(se1, se2, type1)
            else
                let str1 = U.string_of_datatype type1 in
                let str2 = U.string_of_datatype type2 in
                raise (E.AssignmentTypeMismatch(str1, str2))

(* TODO: Investigate Dice differences *)
and check_call s e_l env =
    (* Add the correct activation record if the function takes one *)
    let se_l = expr_list_to_sexpr_list e_l env in
    let record_to_pass = StringMap.find env.env_record_to_pass s in
    let se_l = match record_to_pass with
        Some(tuple) ->
            let record_name = fst tuple in
            let record_type = snd tuple in
            let se = SId(record_name, record_type) in
            se :: se_l
      | None -> se_l
    in
    try
        (* Call the function if it is not a var *)
        let fdecl = StringMap.find_exn env.env_fmap s in
        let return_t = fdecl.return_t in
        let sid = SId(s, fdecl.ftype) in
        SCall(sid, se_l, return_t, 0)
    with | Not_found ->
        try
            (* Get the function pointer if it is a var *)
            let rhs_type = StringMap.find_exn env.env_named_vars s in
            let return_t = match rhs_type with
                Functiontype(_, return_t) -> return_t
              | data_t ->
                    let data_t = U.string_of_datatype data_t in
                    raise (E.CallFailedOnType data_t)
            in
            let env_fname = get_fname_exn env.env_fname in
            let record_type = Datatype(Object_t(env_fname ^ ".record")) in
            let record_type_name = env_fname ^ ".record" in
            let record_name = env_fname ^ "_record" in
            let record_class = StringMap.find_exn env.env_cmap record_type_name in
            let lhs = SId(record_name, record_type) in
            let rhs = SId(s, rhs_type) in
            let sstmt = SObjAccess(lhs, rhs, rhs_type) in
            SCall(sstmt, se_l, return_t, 0)
        with | Not_found -> raise (E.UndefinedFunction s)

and expr_list_to_sexpr_list e_l env = match e_l with
    hd :: tl ->
        let (se, env) = expr_to_sexpr hd env in
        se :: expr_list_to_sexpr_list tl env
  | [] -> []

and check_array_access e e_l env =
    let (se, _) = expr_to_sexpr e env in
    let data_t = sexpr_to_type_exn se in
    let se_l = expr_list_to_sexpr_list e_l env in

    (* Check that the indice parameters are all Int_t *)
    let check_access_params = List.map se_l
        ~f:(fun se -> match (sexpr_to_type_exn se) with
            Datatype(Int_t) -> ()
          | _ -> raise (E.ArrayAccess "Passed non-Int Indice Argument"))
    in

    (* Check that # dims matches # indices *)
    let arr_num_indices = List.length e_l in
    let arr_num_dims = match data_t with
        Arraytype(_, n) -> n
      | _ -> raise (E.ArrayAccess "Passed non-Arraytype Variable")
    in
    let check_num_dims_indices = if arr_num_dims <> arr_num_indices
        then raise (E.ArrayAccess "Number Indices != Number Dimensions")
    in
    SArrayAccess(se, se_l, data_t)

and check_array_create d e_l env =
    let se_l = expr_list_to_sexpr_list e_l env in

    (* Check that the indice parameters are all Int_t *)
    let check_access_params = List.map se_l
        ~f:(fun se -> match (sexpr_to_type_exn se) with
            Datatype(Int_t) -> ()
          | _ -> raise (E.NonIntegerArraySize))
    in

    let arr_num_indices = List.length e_l in
    let convert_d_to_arraytype = function
        Datatype(x) -> Arraytype(x, arr_num_indices)
        | _ -> raise (E.NonArrayTypeCreate)
    in
    let sexpr_type = convert_d_to_arraytype d in
    SArrayCreate(d, se_l, sexpr_type)

and check_function_literal fdecl env =
    let f = StringMap.find_exn env.env_fmap (get_fname_exn env.env_fname) in
    let link_type = Some(Datatype(Object_t(f.fname ^ ".record"))) in
    let sfdecl = convert_fdecl_to_sfdecl env.env_fmap env.env_cmap fdecl env.env_named_vars link_type env.env_record_to_pass in
    higher_order_sfdecls := StringMap.add !higher_order_sfdecls ~key:fdecl.fname ~data:sfdecl;
    SFunctionLit(sfdecl.sfname, sfdecl.sftype)

and check_obj_access e1 e2 env =
    let get_cname_exn = function
        Some(cname) -> cname
      | None -> raise E.CannotUseThisKeywordOutsideOfClass
    in
    let check_lhs = function
        This -> SId("this", Datatype(Object_t(get_cname_exn env.env_cname)))
      | Id(s) -> check_record_access s env (* SId(s, get_Id_type s env) *)
      | _ as e -> raise E.LHSofObjectAccessMustBeAccessible
    in
    let check_rhs e2 =
        let id = match e2 with
            Id s -> s
          | _ -> raise E.RHSofObjectAccessMustBeAccessible
        in
        let cname = match (check_lhs e1) with
            SId(_, data_t) -> (match data_t with
                Datatype(Object_t(name)) -> name)
          | SObjAccess(_, _, data_t) -> (match data_t with
                Datatype(Object_t(name)) -> name)
          | _ -> raise E.RHSofObjectAccessMustBeAccessible
        in
        let crecord = StringMap.find_exn env.env_cmap cname in
        try
            match StringMap.find_exn crecord.field_map id with
                Field(_, s, data_t) -> SId(s, data_t)
        with | Not_found -> raise E.UnknownClassVar
    in

    let lhs = check_lhs e1 in
    let lhs_type = sexpr_to_type_exn lhs in
    let rhs = check_rhs e2 in
    let rhs_t = match rhs with
        SId(_, data_t) -> data_t
    in
    SObjAccess(lhs, rhs, rhs_t)

    (*
    StringMap.iter record_class.field_map
        ~f:(fun ~key:s ~data:d -> print_string (s ^ "\n"));

    let link_type = Hashtbl.find access_link_types fname in
    let print =match link_type with
        Some(dt) ->
            print_string ("fname: " ^ fname ^ "\n");
            print_string ("ltype: " ^ U.string_of_datatype dt ^ "\n");
            print_string "===\n"
      | None -> ()
    in
    print;
    *)

(* Follow access links if var defined outside of function *)
and check_record_access s env =
    let fname = get_fname_exn env.env_fname in

    let rec build_lhs_helper fname inner =
        let record_type_name = fname ^ ".record" in
        let record_class = StringMap.find_exn env.env_cmap record_type_name in
        if StringMap.mem record_class.field_map s then
            inner
        else
            let access_link_name = fname ^ "_@link" in
            let access_link_type = Hashtbl.find_exn access_link_types fname in
            let outer_fname = Hashtbl.find_exn access_link_fnames fname in
            let inner = SObjAccess(inner, SId(access_link_name, access_link_type), access_link_type) in
            build_lhs_helper outer_fname inner
    in

    let build_lhs fname =
        let record_name = fname ^ "_record" in
        let record_type_name = fname ^ ".record" in
        let record_class = StringMap.find_exn env.env_cmap record_type_name in
        let record_type = Datatype(Object_t(record_type_name)) in
        try
            (* Access item if it is the current record *)
            let _ = StringMap.find_exn record_class.field_map s in
            let result = SId(record_name, record_type) in
            result

        with | Not_found ->
            (* Access the item through access links otherwise *)
            let access_link_name = fname ^ "_@link" in
            let access_link_type = Hashtbl.find_exn access_link_types fname in
            let outer_fname = Hashtbl.find_exn access_link_fnames fname in
            build_lhs_helper outer_fname
            (SObjAccess(SId(record_name, record_type), SId(access_link_name, access_link_type), access_link_type))
    in
    let lhs = build_lhs fname in

    let rhs_type = StringMap.find_exn env.env_named_vars s in
    let rhs = SId(s, rhs_type) in
    SObjAccess(lhs, rhs, rhs_type)

and arraytype_to_access_type data_t = match data_t with
    Arraytype(p, _) -> Datatype(p)
  | _ -> raise E.UnexpectedType

and sexpr_to_type sexpr = match sexpr with
    SIntLit(_)                  -> Some(Datatype(Int_t))
  | SFloatLit(_)                -> Some(Datatype(Float_t))
  | SBoolLit(_)                 -> Some(Datatype(Bool_t))
  | SCharLit(_)                 -> Some(Datatype(Char_t))
  | SStringLit(_)               -> Some(Arraytype(Char_t, 1))
  | SFunctionLit(_, data_t)     -> Some(data_t)
  | SId(_, data_t)              -> Some(data_t)
  | SBinop(_, _, _, data_t)     -> Some(data_t)
  | SUnop(_, _, data_t)         -> Some(data_t)
  | SCall(_, _, data_t, _)      -> Some(data_t)
  | SObjAccess(_, _, data_t)    -> Some(data_t)
  | SAssign(_, _, data_t)       -> Some(data_t)
  | SArrayAccess(_, _, data_t)  -> Some(arraytype_to_access_type data_t)
  | SArrayCreate(_, _, data_t)  -> Some(data_t)
  | SThis(data_t)               -> Some(data_t)
  | SNoexpr                     -> None

and sexpr_to_type_exn sexpr = match (sexpr_to_type sexpr) with
    Some(t) -> t
  | None -> raise E.UnexpectedNoexpr

(* Statement to SStatement Conversion *)
and check_sblock sl env = match sl with
    [] ->   ([SBlock([SExpr(SNoexpr, Datatype(Unit_t))])], env)
  | _ ->    let (sl,_) = convert_stmt_list_to_sstmt_list sl env in
            ([SBlock(sl)], env)

and check_expr_stmt e env =
    let se, env = expr_to_sexpr e env in
    let data_t = sexpr_to_type_exn se in
    ([SExpr(se, data_t)], env)

and check_return e env =
    let (se, _) = expr_to_sexpr e env in
    let data_t = sexpr_to_type_exn se in
    match data_t, env.env_return_t  with
        (* Allow unit returns for reference types e.g. objects, arrays *)
        Datatype(Unit_t), Datatype(Object_t(_))
      | Datatype(Unit_t), Arraytype(_, _) -> ([SReturn(se, data_t)], env)
      | _ ->
            if data_t = env.env_return_t
            then ([SReturn(se, data_t)], env)
            else raise (E.ReturnTypeMismatch
                (U.string_of_datatype data_t,
                U.string_of_datatype env.env_return_t,
                env.env_fname))

and local_handler s data_t e env =
    if StringMap.mem env.env_named_vars s
    then raise (E.DuplicateVar(s))
    else
        let (se, _) = expr_to_sexpr e env in
        if se = SNoexpr then
            let named_vars = StringMap.add env.env_named_vars
                ~key:s
                ~data:data_t;
            in
            let record_vars = StringMap.add env.env_record_vars
                ~key:s
                ~data:data_t;
            in
            let new_env = {
                env_cname = env.env_cname;
                env_crecord = env.env_crecord;
                env_cmap = env.env_cmap;
                env_fname = env.env_fname;
                env_fmap = env.env_fmap;
                env_named_vars = named_vars;
                env_record_vars = record_vars;
                env_record_to_pass = env.env_record_to_pass;
                env_return_t = env.env_return_t;
                env_in_for = env.env_in_for;
                env_in_while = env.env_in_while;
            }
            in
            let save_obj_with_storage =
                (* Add the temp var as a local *)

                let var_name = ".tmp_malloc_var"^ (string_of_int !seed_index) in
                let var_type = data_t in
                let sstmt_l = [SLocal(var_name, var_type, SNoexpr)] in
                let sstmt_id = SId(var_name, var_type) in
                let sstmt_record_var = check_record_access s new_env in
                let sexpr = SAssign(sstmt_record_var, sstmt_id, var_type) in
                let sstmt_l = SExpr(sexpr, var_type) :: sstmt_l in
                (List.rev sstmt_l, new_env)
            in
            (* Only allocate locals if they need to be allocated (pointer in activation record) *)
            seed_index := !seed_index + 1;

            match data_t with
                    Datatype(Object_t(_)) -> save_obj_with_storage
                  | _ -> ([SExpr(SNoexpr, Datatype(Unit_t))], new_env)
        else
            let se_data_t = sexpr_to_type_exn se in
            let is_assignable = function
                NoFunctiontype
              | Any -> false
              | _ -> true
            in
            let valid_assignment = function
                (Any, _) -> is_assignable se_data_t
              | (data_t, se_data_t) -> if data_t = se_data_t
                    then true else false
            in
            if valid_assignment (data_t, se_data_t)
            then
                let named_vars = StringMap.add env.env_named_vars
                    ~key:s
                    ~data:se_data_t;
                in
                let record_vars = StringMap.add env.env_record_vars
                    ~key:s
                    ~data:se_data_t;
                in

                (* Record to pass *)
                let record_to_pass = match se with
                    SFunctionLit(_,_) ->
                        let data = (get_fname_exn env.env_fname ^ "_record", Datatype(Object_t(get_fname_exn env.env_fname ^ ".record"))) in
                        StringMap.add env.env_record_to_pass
                            ~key:s
                            ~data:data
                  | _ -> env.env_record_to_pass
                in

                let new_env = {
                    env_cname = env.env_cname;
                    env_crecord = env.env_crecord;
                    env_cmap = env.env_cmap;
                    env_fname = env.env_fname;
                    env_fmap = env.env_fmap;
                    env_named_vars = named_vars;
                    env_record_vars = record_vars;
                    env_record_to_pass = record_to_pass;
                    env_return_t = env.env_return_t;
                    env_in_for = env.env_in_for;
                    env_in_while = env.env_in_while;
                }
                in
                let save_object_no_storage =
                    let lhs = check_record_access s new_env in
                    let sexpr = SAssign(lhs, se, se_data_t) in
                    let sstmt = SExpr(sexpr, se_data_t) in
                    ([sstmt], new_env)
                in
                save_object_no_storage

                (* (SLocal(s, se_data_t, se), new_env) *)
            else
                let type1 = U.string_of_datatype data_t in
                let type2 = U.string_of_datatype se_data_t in
                raise (E.LocalAssignmentTypeMismatch(type1, type2))

and parse_stmt stmt env = match stmt with
    Block sl                -> check_sblock sl env
  | Expr e                  -> check_expr_stmt e env
  | Return e                -> check_return e env
  | Local(s, data_t, e)     -> local_handler s data_t e env
  | If(e, s1, s2)           -> check_if e s1 s2 env
  | For(e1, e2, e3, s)      -> check_for e1 e2 e3 s env
  | While(e, s)             -> check_while e s env
  | Break                   -> check_break env
  | Continue                -> check_continue env

(* Semantically check a list of stmts; Convert to sstmts *)
and convert_stmt_list_to_sstmt_list sl env =
    let env_ref = ref(env) in
    let rec iter = function
        head :: tail ->
            let (a_head, env) = parse_stmt head !env_ref in
            env_ref := env;
            a_head @ (iter tail)
      | [] -> []
    in
    let sstmt_list = ((iter sl), !env_ref) in
    sstmt_list

and check_if e s1 s2 env =
    let (se, _) = expr_to_sexpr e env in
    let t = sexpr_to_type_exn se in
    let (ifbody, _) = parse_stmt s1 env in
    let (elsebody, _) = parse_stmt s2 env in
    if t = Datatype(Bool_t)
        then ([SIf(se, SBlock(ifbody), SBlock(elsebody))], env)
        else raise E.InvalidIfStatementType

and check_for e1 e2 e3 s env =
    let old_in_for = env.env_in_for in
    let env = update_call_stack true env.env_in_while env in
    let (se1,_) = expr_to_sexpr e1 env in
    let (se2,_) = expr_to_sexpr e2 env in
    let (se3,_) = expr_to_sexpr e3 env in
    let (sbody,_) = parse_stmt s env in
    let conditional_t = sexpr_to_type_exn se2 in
    let sfor =
        if conditional_t = Datatype(Bool_t)
            then SFor(se1, se2, se3, SBlock(sbody))
            else raise E.InvalidForStatementType
    in
    let env = update_call_stack old_in_for env.env_in_while env in
    ([sfor], env)

and check_while e s env =
    let old_in_while = env.env_in_while in
    let env = update_call_stack env.env_in_for true env in
    let (se,_) = expr_to_sexpr e env in
    let conditional_t = sexpr_to_type_exn se in
    let (sbody,_) = parse_stmt s env in
    let swhile =
        if conditional_t = Datatype(Bool_t)
            then SWhile(se, SBlock(sbody))
            else raise E.InvalidWhileStatementType
    in
    let env = update_call_stack env.env_in_for old_in_while env in
    ([swhile], env)

and check_break env =
    if env.env_in_for || env.env_in_while then
        ([SBreak], env)
    else raise E.BreakOutsideOfLoop

and check_continue env =
    if env.env_in_for || env.env_in_while then
        ([SContinue], env)
    else raise E.ContinueOustideOfLoop

(* Map Generation *)
(* ============== *)

(* Generate StringMap: cname -> crecord *)
and build_crecord_map fmap cdecls fdecls =
    (* Check each constituent of a class: fields, member functions, constructors *)
    let helper m (cdecl : Ast.cdecl) =
        (* Check Fields *)
        let check_fields m field =  match field with
        Field(scope, s, data_t) ->
            if StringMap.mem m s then raise (E.DuplicateField s)
            else StringMap.add m ~key:s ~data:(Field(scope, s, data_t))
        in
        (* Check Methods *)
        let method_name = get_method_name cdecl.cname in
        let check_methods m fdecl =
            if StringMap.mem m (method_name fdecl)
                then raise (E.DuplicateFunctionName (method_name fdecl))
            else if (StringMap.mem fmap fdecl.fname)
                then raise (E.FunctionNameReserved fdecl.fname)
            else StringMap.add m ~key:(method_name fdecl) ~data:fdecl
        in
        (* Check Class Name *)
        if (StringMap.mem m cdecl.cname) then raise (E.DuplicateClassName(cdecl.cname))
        (* Add Class Record to Map *)
        else StringMap.add m
            ~key:cdecl.cname
            ~data:({
                field_map = List.fold_left cdecl.cbody.fields
                    ~f:check_fields
                    ~init:StringMap.empty;
                method_map = List.fold_left cdecl.cbody.methods
                    ~f:check_methods
                    ~init:StringMap.empty;
                cdecl = cdecl
            })
    in
    let crecord_map = List.fold_left cdecls
        ~f:helper
        ~init:StringMap.empty
    in

    (* Add function Records *)
    let discover_named_vars fdecl =
        let field_map = List.fold fdecl.formals
            ~f:(fun m formal -> match formal with
                Formal(s, d) -> (StringMap.add m ~key:s ~data:(Field(Public, s, d))))
            ~init:StringMap.empty
        in
        let helper stmt = match stmt with
            Local(s, d, _) -> Some(s, Field(Public, s, d))
          | _ -> None
        in
        List.fold fdecl.body
            ~f:(fun m stmt -> match (helper stmt) with
                Some(t) -> StringMap.add m ~key:(fst t) ~data:(snd t)
              | None -> m)
            ~init:field_map
    in
    let fhelper m (fdecl : Ast.fdecl) =
        let field_map = discover_named_vars fdecl in
        let field_map =
            try
                let link_type = Hashtbl.find_exn access_link_types fdecl.fname in
                let link_name = fdecl.fname ^ "_@link" in
                let field = Field(Public, link_name, link_type) in
                StringMap.add field_map ~key:link_name ~data:field
            with | Not_found -> field_map
        in
        let temp_class =    ({
            field_map = field_map;
            method_map = StringMap.empty;
            cdecl = ({
                cname = fdecl.fname ^ ".record";
                extends = NoParent;
                cbody = ({ fields = []; methods = []; })
            })
        })
        in
        StringMap.add m
            ~key:(fdecl.fname ^ ".record")
            ~data:temp_class
    in
    List.fold_left fdecls
        ~f:fhelper
        ~init:crecord_map

(* Generate StringMap: fname -> fdecl *)
and build_fdecl_map reserved_sfdecl_map first_order_fdecls =
    (* Check whether each function is already defined before adding it to the map *)
    let check_functions m fdecl =
        if StringMap.mem m fdecl.fname
            then raise (E.DuplicateFunctionName fdecl.fname)
        else if StringMap.mem reserved_sfdecl_map fdecl.fname
            then raise (E.FunctionNameReserved fdecl.fname)
        else StringMap.add m ~key:(fdecl.fname) ~data:fdecl
    in

    (* Add all the first order functions to the map *)
    let map = List.fold_left first_order_fdecls
        ~f:check_functions
        ~init:StringMap.empty;
    in

    (* DFS to discover all higher-order functions *)
    let rec discover_higher_order l fdecl =
        let check_higher_order_helper l stmt = match stmt with
            Local(_, _, e) -> (match e with
                FunctionLit(nested_fdecl) ->
                    let link_t = Datatype(Object_t(fdecl.fname ^ ".record")) in
                    Hashtbl.add_exn access_link_types
                        ~key:nested_fdecl.fname
                        ~data:link_t;
                    Hashtbl.add_exn access_link_fnames
                        ~key:nested_fdecl.fname
                        ~data:fdecl.fname;
                    nested_fdecl :: discover_higher_order l nested_fdecl
              | _ -> l)
          | _ -> l
        in
        List.fold_left fdecl.body
            ~f:check_higher_order_helper
            ~init:l
    in
    let higher_order_fdecls = List.fold_left first_order_fdecls
        ~f:discover_higher_order
        ~init:[]
    in

    (* Add all the higher order functions to the map *)
    let map = List.fold_left higher_order_fdecls
        ~f:check_functions
        ~init:map;
    in

    (* Add reserved functions to the map *)
    let add_reserved_fdecls m key =
        let sfdecl = StringMap.find_exn reserved_sfdecl_map key in
        let fdecl = {
            fname = key;
            ftype = sfdecl.sftype;
            return_t = sfdecl.sreturn_t;
            formals = sfdecl.sformals;
            body = [];
            scope = Public;
            overrides = false;
            root_cname = None;
        }
        in
        StringMap.add m ~key:key ~data:fdecl
    in
    let fdecl_map = List.fold_left (StringMap.keys reserved_sfdecl_map)
        ~f:add_reserved_fdecls
        ~init:map
    in
    let fdecls_to_generate = first_order_fdecls @ higher_order_fdecls
    in
    (fdecl_map, fdecls_to_generate, first_order_fdecls, higher_order_fdecls)

(* Conversion *)
(* ========== *)

(* Convert a method to a semantically checked function *)
(* Name = <root_class>.<fname> *)
(* Prepend instance of class to function parameters *)
and convert_method_to_sfdecl fmap cmap cname fdecl =
    let crecord = StringMap.find_exn cmap cname
    in
    let root_cname = match fdecl.root_cname with
        Some(c) -> c
      | None -> cname
    in
    (* The class that the function takes as an additional formal *)
    let class_formal =
        if fdecl.overrides then
            Ast.Formal("this", Datatype(Object_t(root_cname)))
        else
            Ast.Formal("this", Datatype(Object_t(cname)))
    in
    let env_param_helper m formal = match formal with
        Formal(s, data_t) -> (StringMap.add m ~key:s ~data:formal)
      | _ -> m
    in
    let env_params = List.fold_left (class_formal :: fdecl.formals)
        ~f:env_param_helper
        ~init:StringMap.empty
    in
    let env = {
        env_cname       = Some(cname);
        env_crecord     = Some(crecord);
        env_cmap        = cmap;
        env_fname       = None;
        env_fmap        = fmap;
        env_named_vars  = StringMap.empty;
        env_record_vars = StringMap.empty;
        env_record_to_pass = StringMap.empty;
        env_return_t    = fdecl.return_t;
        env_in_for      = false;
        env_in_while    = false;
    }
    in
    (* Assign fname to <fname> or <class>.<fname> appropriately *)
    let fname = get_method_name cname fdecl
    in
    (* Prepend the class as the first parameter to the function if it is a method *)
    let fdecl_formals = class_formal :: fdecl.formals
    in
    (* Check the stmts in the fbody *)
    let (fbody, env) = convert_stmt_list_to_sstmt_list fdecl.body env
    in
    let record_vars = StringMap.fold env.env_record_vars
        ~f:(fun ~key:k ~data:data_t l -> (k,data_t) :: l)
        ~init:[]
    in
    {
        sfname          = fname;
        sreturn_t       = fdecl.return_t;
        srecord_vars    = record_vars;
        sformals        = fdecl_formals;
        sbody           = fbody;
        fgroup          = Sast.User;
        overrides       = fdecl.overrides;
        source          = Some(cname);
        sftype          = fdecl.ftype;
    }

(* Convert a function to a semantically checked function *)
and convert_fdecl_to_sfdecl fmap cmap fdecl named_vars link_type record_to_pass =

    (* Add access link, if the function is not first class *)
    let sformals = match link_type with
        Some(t) -> let access_link = Formal(fdecl.fname ^ "_@link", t) in access_link :: fdecl.formals
      | None -> fdecl.formals
    in
    (*
    let sformals = fdecl.formals in
    *)

    (* Add named values to env *)
    let env_param_helper m formal = match formal with
        Formal(s, data_t) ->
            if StringMap.mem named_vars s
            then raise (E.DuplicateVar s)
            else StringMap.add m ~key:s ~data:data_t
      | _ -> m
    in
    let named_vars = List.fold_left sformals
        ~f:env_param_helper
        ~init:named_vars
    in
    let record_vars = List.fold_left sformals
        ~f:env_param_helper
        ~init:StringMap.empty
    in

    (* Ensure function has the correct return type *)
    (* If the function returns a function, the first parameter is the returning function's activation record *)
    let record_type = Datatype(Object_t(fdecl.fname ^ ".record")) in

    let sreturn_t = match fdecl.return_t with
        Functiontype(dt_l, dt) -> Functiontype(record_type :: dt_l, dt)
      | _ as return_t -> return_t
    in

    let env = {
        env_cname       = None;
        env_crecord     = None;
        env_cmap        = cmap;
        env_fname       = Some(fdecl.fname);
        env_fmap        = fmap;
        env_named_vars  = named_vars;
        env_record_vars = record_vars;
        env_record_to_pass = record_to_pass;
        env_return_t    = sreturn_t;
        env_in_for      = false;
        env_in_while    = false;
    }
    in

    (* Check the stmts in the fbody *)
    let (sfbody, env) = convert_stmt_list_to_sstmt_list fdecl.body env
    in
    let record_vars = StringMap.fold env.env_record_vars
        ~f:(fun ~key:k ~data:data_t l -> (k,data_t) :: l)
        ~init:[]
    in
    let srecord_vars = match link_type with
        Some(t) -> let access_link = (fdecl.fname ^ "_@link", t) in access_link :: record_vars
      | None -> record_vars
    in

    (* Assign any parameters to their corresponding activation record vars *)
    let field_helper l f = match f with
        Formal(s, data_t) ->
            let sstmt_id = SId(s, data_t) in
            let sstmt_record_var = check_record_access s env in
            let sexpr = SAssign(sstmt_record_var, sstmt_id, data_t) in
            SExpr(sexpr, data_t) :: l
      | _ -> l
    in

    let sfbody = List.fold_left sformals
        ~f:field_helper
        ~init:sfbody
    in

    (* Add activation record *)
    let record_type = Datatype(Object_t(fdecl.fname ^ ".record")) in
    let record_name = fdecl.fname ^ "_record" in
    let sfbody = SLocal(record_name, record_type, SNoexpr) :: sfbody in

    (* Make sure the function has the correct type (prepend access link) *)
    let sftype = match link_type with
        Some(t) -> (match fdecl.ftype with
            Functiontype(dt_l, dt) -> Functiontype(t :: dt_l, dt)
          | _ -> raise E.FTypeMustBeFunctiontype)
      | None -> fdecl.ftype
    in
    {
        sfname          = fdecl.fname;
        sreturn_t       = sreturn_t;
        srecord_vars    = record_vars;
        sformals        = sformals;
        sbody           = sfbody;
        fgroup          = Sast.User;
        overrides       = fdecl.overrides;
        source          = None;
        sftype          = sftype;
    }

(* Generate activation records for fdecls *)
let generate_sfdecl_records sfdecl =
    let fields = List.map sfdecl.srecord_vars
        ~f:(function (s, data_t) -> Field(Public, s, data_t))
    in
    {
        scname = sfdecl.sfname ^ ".record";
        sfields = fields;
        sfdecls = [];
    }

(* Convert cdecls to scdecls *)
let convert_cdecl_to_scdecl sfdecls (c:Ast.cdecl) =
    {
        scname = c.cname;
        sfields = c.cbody.fields;
        sfdecls = sfdecls;
    }

(* Generate Sast: sprogram *)
let convert_ast_to_sast
    crecord_map (cdecls : cdecl list)
    fdecl_map (first_order_fdecls : fdecl list) (higher_order_fdecls : fdecl list) =
    let is_main = (fun f -> match f.sfname with s -> s = "main") in
    let get_main fdecls =
        let mains = (List.filter ~f:is_main fdecls)
        in
        if List.length mains < 1 then
            raise E.MissingMainFunction
        else if List.length mains > 1 then
            raise E.MultipleMainFunctions
        else
            List.hd_exn mains
    in
    let remove_main fdecls =
        List.filter ~f:(fun f -> not (is_main f)) fdecls
    in
    let handle_cdecl cdecl =
        let crecord = StringMap.find_exn crecord_map cdecl.cname in
        let sfdecls = List.fold_left cdecl.cbody.methods
            ~f:(fun l f -> (convert_method_to_sfdecl fdecl_map crecord_map cdecl.cname f) :: l)
            ~init:[]
        in
        let sfdecls = remove_main sfdecls in
        let scdecl = convert_cdecl_to_scdecl sfdecls cdecl in
        (scdecl, sfdecls)
    in
    let iter_cdecls t c =
        let scdecl = handle_cdecl c in
        (fst scdecl :: fst t, snd scdecl @ snd t)
    in
    let (scdecl_list, sfdecl_list) = List.fold_left cdecls
        ~f:iter_cdecls
        ~init:([], [])
    in

    (* Append first order fdecls to the tuple *)
    let sfdecls = List.fold_left first_order_fdecls
        ~f:(fun l f -> (convert_fdecl_to_sfdecl fdecl_map crecord_map f StringMap.empty None StringMap.empty) :: l)
        ~init:[]
    in

    (* Append higher order fdecls to the tuple *)
    let sfdecls = StringMap.fold !higher_order_sfdecls
        ~f:(fun ~key:k ~data:sfdecl l -> sfdecl :: l)
        ~init:sfdecls
    in
    let (scdecl_list, sfdecl_list) = (scdecl_list, sfdecls @ sfdecl_list) in

    (* Add Activation Record structs to the tuple *)
    let scdecls = List.fold_left sfdecl_list
        ~f:(fun l f -> (generate_sfdecl_records f) :: l)
        ~init:[]
    in
    let (scdecl_list, sfdecl_list) = (scdecls @ scdecl_list, sfdecl_list) in

    let main = get_main sfdecl_list in
    let sfdecl_list = remove_main sfdecl_list in
    {
        classes     = scdecl_list;
        functions   = sfdecl_list;
        main        = main;
    }

(* Analyze *)
(* TODO: Include code from external files *)
let analyze filename ast = match ast with
    Program(includes, specs, cdecls, fdecls) ->
        (* Create sfdecl list of builtin LLVM functions *)
        let reserved_map = build_reserved_map in
        (* Create StringMap: fname -> fdecl of functions *)
        let (fdecl_map, fdecls, first, higher) = build_fdecl_map reserved_map fdecls in
        (* Create StringMap: cname -> cdecl of classes *)
        let crecord_map = build_crecord_map reserved_map cdecls fdecls in
        (* Generate sast: sprogram *)
        let sast = convert_ast_to_sast crecord_map cdecls fdecl_map first higher in
        sast
