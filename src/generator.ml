open Parser
module E = Exceptions

type token_attr = {
    lineno : int;
    cnum : int;
}

let filename_ref = ref ""
let lineno_ref = ref 1
let cnum_ref = ref 1
let last_token_ref = ref EOF

(* Build an OCaml List of the tokens returned from the Scanner *)
let build_token_list filename lexbuf =
    Scanner.filename := filename;
    let rec helper lexbuf token_list =
        let token = Scanner.token lexbuf in
        let lineno = !Scanner.lineno in
        let cnum = (Lexing.lexeme_start_p lexbuf).Lexing.pos_cnum in
        match token with
            EOF as eof  -> (eof, { lineno = lineno; cnum = cnum }) :: token_list
          | t           -> (t, {lineno = lineno; cnum = cnum}) :: helper lexbuf token_list
    in
    helper lexbuf []

(* Build an AST by feeding the Scanner's tokens into the Parser *)
let build_ast filename token_list =
    let token_list = ref(token_list) in
    let tokenizer _ =
        match !token_list with
            (head, attr) :: tail -> 
                filename_ref := filename;
                lineno_ref := attr.lineno;
                cnum_ref := attr.cnum;
                last_token_ref := head;
                token_list := tail; 
                head
          | [] -> raise E.MissingEOF
    in
    let program = Parser.program tokenizer (Lexing.from_string "") in
    program
