(* Stop Compiler Top Level *)

(* Attributions: *)
    (* Professor Stephen A. Edwards, MicroC Compiler *)
        (* http://www.cs.columbia.edu/~sedwards/ *)
    (* David Watkins, Dice Compiler *)
    (* Jeff Lee, C Language Yacc Grammar *)
        (* https://www.lysator.liu.se/c/ANSI-C-grammar-y.html#translation-unit *)

open Core.Std 

module A = Analysis
module C = Codegen
module E = Exceptions
module G = Generator
module L = Llvm
module P = Parser
module S = Scanner
module U = Utils

(* Compile <src> <destination> *)
type action = Tokens | Ast | Sast 
            | CompileStdinStdout| CompileStdinFile
            | CompileFileStdout | CompileFileFile 
            | Help

let get_action = function
    "-t"    -> Tokens
  | "-a"    -> Ast
  | "-s"    -> Sast
  | "-css"  -> CompileStdinStdout
  | "-csf"  -> CompileStdinFile
  | "-cfs"  -> CompileFileStdout
  | "-cff"  -> CompileFileFile
  | "-h"    -> Help
  | _ as s  -> raise (E.InvalidOption s)

let check_single_argument = function
      "-h"      -> (Help, "")
    | "-tendl"
    | "-t"
    | "-a"
    | "-s"
    | "-c"
    | "-cfs"    -> raise (E.NoFileArgument)
    | "-cff"
    | _ as s  -> (CompileFileStdout, s)

let help_string = (
      "Usage: stop [-option] <source file>\n" ^
        "-option: (defaults to \"-css\")\n" ^
        "\t-t: Print tokens\n" ^
        "\t-a: Prints AST\n" ^
        "\t-s: Prints SAST\n" ^
        "\t-css: Compiles stdin to stdout \n" ^
        "\t-csf: Compiles stdin to file\n" ^
        "\t-cfs: Compiles file to stdout (<filename>.<ext>)\n" ^
        "\t-cff: Compiles file to file (<filename>.<ext> -> <filename>.ll)\n" ^
        "\t-h: Print help\n"
    )

let stop_name filename =
    let basename = Filename.basename filename in
    let filename = Filename.chop_extension basename in
    filename ^ ".ll"

let _ = 
    ignore(Printexc.record_backtrace true);
    try
        (* Get the Appropriate Action *)
        let (action, filename) = 
            if Array.length Sys.argv = 1 then
                CompileStdinStdout, ""
            else if Array.length Sys.argv = 2 then
                check_single_argument (Sys.argv.(1))
            else if Array.length Sys.argv = 3 then
                get_action Sys.argv.(1), Sys.argv.(2)
            else raise E.InvalidArgc
        in 

        (* Iterative Application of each Compilation Phase *)
        (* Each phase is defined as a function which is only called when needed *)
        let file_in () = if filename = "" then stdin else open_in filename in 
        let lexbuf () = Lexing.from_channel (file_in ()) in
        let token_list () = G.build_token_list filename (lexbuf ()) in
        let ast () = G.build_ast filename (token_list ()) in
        let sast () = A.analyze filename (ast ()) in
        let llm () = C.codegen_sast (sast ()) in

        (* Respond Appropriately to Action *)
        match action with
            Tokens              -> print_string (U.token_list_to_string (token_list ()))
          | Ast                 -> print_string (U.string_of_program (ast()))
          | Sast                -> print_string (U.string_of_sprogram (sast()))
          | CompileStdinStdout
(*        | CompileFileStdout   -> sast (); print_string "test" *)
          | CompileFileStdout   -> print_string (L.string_of_llmodule (llm ()))
          | CompileStdinFile
          | CompileFileFile     -> L.print_module (stop_name filename) (llm ())
          | Help                -> print_string help_string
    with 
        (* Deal with Exceptions *)
        E.IllegalCharacter(file, c, ln) -> 
            print_string 
                ("Illegal character '" ^ c ^ "' in line "
                ^ string_of_int ln ^ " of " ^ file ^ "\n")
      | Parsing.Parse_error ->
            print_string
                ("Syntax Error:\n"
                ^ U.error_string_of_file !G.filename_ref
                ^ ", line " ^ string_of_int !G.lineno_ref
                ^ ", characters " ^ U.error_string_of_cnum !G.cnum_ref !G.last_token_ref
                ^ ", Token " ^ U.string_of_token !G.last_token_ref ^ "\n")
      | _ as e -> raise e

(*
            Compile in
        let lexbuf = Lexing.from_channel stdin in 
        let ast = Parser.program Scanner.token lexbuf in
        Semant.check ast;
        match action with
          | LLVM_IR -> print_string (Llvm.string_of_llmodule (Codegen.translate ast))
          | Compile -> let m = Codegen.translate ast in
          Llvm_analysis.assert_valid_module m;
          print_string (Llvm.string_of_llmodule m)
*)
