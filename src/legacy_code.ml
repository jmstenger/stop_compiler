(* Legacy Code *)

(* Old Codegen *)
let translate ast = match ast with
    A.Program(includess, specs, classes, functions) -> 
    let context     = L.global_context () in 
    let the_module  = L.create_module context "Stop"
    and i32_t       = L.i32_type    context
    and i8_t        = L.i8_type     context
    and i1_t        = L.i1_type     context 
    and str_t       = L.pointer_type (L.i8_type context)
    and void_t      = L.void_type   context in

    let str_type = A.Arraytype(A.Char_t, 1) in 

    let ltype_of_prim = function
        A.Int_t ->          i32_t
      | A.Float_t ->        i32_t
      | A.Bool_t ->         i1_t
      | A.Char_t ->         i8_t
      (* TODO: Implement find_struct function for Object_t *)
      | A.Unit_t ->         void_t
    in

    let rec ltype_of_arraytype arraytype = match arraytype with
        A.Arraytype(p, 1) -> L.pointer_type (ltype_of_prim p)
      | A.Arraytype(p, i) -> 
            L.pointer_type (ltype_of_arraytype (A.Arraytype(p, i-1)))
      | _ -> raise(E.InvalidStructType "Array Pointer Type")
    in

    let ltype_of_datatype = function
        A.Datatype(p) -> ltype_of_prim p
      | A.Arraytype(p, i) -> ltype_of_arraytype (A.Arraytype(p,i)) in

    let ltype_of_formal = function
        A.Formal(data_t, s) -> ltype_of_datatype data_t in
    
    let atype_of_datatype = function
        A.Datatype(p) -> p 
      | A.Arraytype(p, i) -> p in

    (* Declare printf(), which the print built-in function will call *)
    (* printf() is already implemented in LLVM *)
    let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
    let printf_func = L.declare_function "printf" printf_t the_module in

    (* Define each function (arguments and return type) so we can call it *)
    let function_decls =
        let function_decl m fdecl =
            let name = fdecl.A.fname
            and formal_types =
        Array.of_list (List.map (fun formal -> ltype_of_formal formal) fdecl.A.formals)
            in let ftype = L.function_type (ltype_of_datatype fdecl.A.return_t) formal_types in
            StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in

    (* Fill in the body of the given function *)
    let build_function_body fdecl =
        let (the_function, _) = StringMap.find fdecl.A.fname function_decls in
        let builder = L.builder_at_end context (L.entry_block the_function) in

        let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in

        (* Construct code for an expression; return its value *)
        let rec expr builder = function
            A.IntLit i -> L.const_int i32_t i
          | A.FloatLit f -> L.const_float i32_t f
          | A.BoolLit b -> L.const_int i1_t (if b then 1 else 0)
          | A.CharLit c -> L.const_int i8_t (Char.code c)
          | A.StringLit s -> L.build_global_stringptr s "tmp" builder
          | A.Id s -> raise E.NotImplemented
          | A.Binop (e1, op, e2) -> build_binop e1 op e2
          | A.Unop(op, e) -> build_unop op e
          | A.Call ("printf", e) -> build_printf e
          | A.Call (s, e) -> raise E.NotImplemented
          | A.Noexpr -> L.const_int i32_t 0

        and build_binop e1 op e2 =
            let e1' = expr builder e1
            and e2' = expr builder e2 in
                (match op with
                    A.Add       -> L.build_add
                  | A.Sub       -> L.build_sub
                  | A.Mult      -> L.build_mul
                  | A.Div       -> L.build_sdiv
                  | A.And       -> L.build_and
                  | A.Or        -> L.build_or
                  | A.Equal     -> L.build_icmp L.Icmp.Eq
                  | A.Neq       -> L.build_icmp L.Icmp.Ne
                  | A.Less      -> L.build_icmp L.Icmp.Slt
                  | A.Leq       -> L.build_icmp L.Icmp.Sle
                  | A.Greater   -> L.build_icmp L.Icmp.Sgt
                  | A.Geq       -> L.build_icmp L.Icmp.Sge)
            e1' e2' "tmp" builder

        and build_unop op e =
            let e' = expr builder e in
                (match op with
                    A.Neg       -> L.build_neg
                  | A.Not       -> L.build_not)
                e' "tmp" builder

        and build_printf e =
            let format_str = match e with
                [] -> A.Noexpr
              | hd :: tl -> hd
            and args = match e with
                [] -> []
              | hd :: tl -> tl
            in
            let first_arg = match args with
                [] -> A.Noexpr
              | hd :: tl -> hd
            in
            let format_lstr = match format_str with
                A.StringLit(s) -> L.build_global_stringptr s "fmt" builder
              | _ -> raise E.PrintfFirstArgNotString
            in
            let l_format_args_list = List.map (expr builder) args 
            in
            let l_full_args_list = [format_lstr] @ l_format_args_list
            in
            let l_args_arr = Array.of_list l_full_args_list
            in
            L.build_call printf_func l_args_arr "printf" builder
        in

        (* Invoke "f builder" if the current block doesn't already
           have a terminal (e.g., a branch). *)
        let add_terminal builder f =
            match L.block_terminator (L.insertion_block builder) with
                Some _ -> ()
              | None -> ignore (f builder) in

        (* Build the code for the given statement; return the builder for
           the statement's successor *)
        let rec stmt builder = function
            A.Block sl -> List.fold_left stmt builder sl
          | A.Expr e -> ignore (expr builder e); builder 
          | A.Return e -> build_sreturn e
          | A.If (predicate, then_stmt, else_stmt) -> build_sif predicate then_stmt else_stmt
          | A.While(predicate, body) -> build_swhile predicate body
          | A.For (e1, e2, e3, body) -> build_sfor e1 e2 e3 body
        and build_sreturn e =
            ignore (match  fdecl.A.return_t with
                A.Datatype(A.Unit_t) -> L.build_ret_void builder
              | _ -> L.build_ret (expr builder e) builder
            );
            builder

        and build_sif predicate then_stmt else_stmt =
            let bool_val = expr builder predicate in
            let merge_bb = L.append_block context "merge" the_function in
            let then_bb = L.append_block context "then" the_function in
            add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
                (L.build_br merge_bb);
            let else_bb = L.append_block context "else" the_function in
            add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
                (L.build_br merge_bb);
            ignore (L.build_cond_br bool_val then_bb else_bb builder);
            L.builder_at_end context merge_bb

        and build_swhile predicate body = 
            let pred_bb = L.append_block context "while" the_function in
            ignore (L.build_br pred_bb builder);
            let body_bb = L.append_block context "while_body" the_function in
            add_terminal (stmt (L.builder_at_end context body_bb) body)
                (L.build_br pred_bb);
            let pred_builder = L.builder_at_end context pred_bb in
            let bool_val = expr pred_builder predicate in
            let merge_bb = L.append_block context "merge" the_function in
            ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
            L.builder_at_end context merge_bb

        and build_sfor e1 e2 e3 body =
            stmt builder (A.Block [A.Expr(e1); A.While(e2, A.Block [body; A.Expr(e3)])] )
        in

        (* Build the code for each statement in the function *)
        let builder = stmt builder (A.Block fdecl.A.body) in

        (* Add a return if the last block falls off the end *)
        add_terminal builder (match fdecl.A.return_t with
            A.Datatype(A.Unit_t) -> L.build_ret_void
          | data_t -> L.build_ret (L.const_int (ltype_of_datatype data_t) 0)
        )
    in

    List.iter build_function_body functions;
    the_module
