type token = 
    | TRUE
    | FALSE
    | IDENT of string
    | AND
    | OR
    | XOR
    | EQV
    | NOT
    | LPAREN
    | RPAREN
    | EOL

    let string_of_token (tok: token) : string = 
    match tok with
    | TRUE -> "1"
    | FALSE -> "0"
    | IDENT s -> "\"" ^ s ^ "\""
    | AND -> "and"
    | OR -> "or"
    | XOR -> "xor"
    | EQV -> "eqv"
    | NOT -> "not"
    | LPAREN -> "("
    | RPAREN -> ")"
    | EOL -> "eol"

let rec print_token_list (inp : token list) =
    match inp with
    | [] -> ()
    | a :: r -> print_string (string_of_token a ^ " ") ; print_token_list r


let rec lexer (input : string) : token list = 
    let exception Lexer_error in
    if String.length input = 0 then [EOL] 
    else if (String.get input 0 = ' ') || (String.get input 0 = '\t')
        then if String.length input = 1 
            then []
            else lexer (String.sub input 1 (String.length input - 1))
    else

    let compile_start (pattern: Re.t) : Re.re = 
        Re.compile (Re.seq [Re.start; pattern])
    in
    let rules : (Re.re * token) list = [
        (compile_start (Re.char '\n'), EOL);
        (compile_start (Re.alt [Re.str "true"; Re.char '1']), TRUE);
        (compile_start (Re.alt [Re.str "false"; Re.char '0']), FALSE);
        (compile_start (Re.char '*'), AND);
        (compile_start (Re.char '+'), OR);
        (compile_start (Re.char '#'), XOR);
        (compile_start (Re.str "::"), EQV);
        (compile_start (Re.char '~'),  NOT);
        (compile_start (Re.char '('),  LPAREN);
        (compile_start (Re.char ')'),  RPAREN)
    ] in
    let (new_token, consumed_len) : token * int = (
        let tokenizer ((pattern, tok) : Re.re * token) : (token * int) option =
            if Re.execp pattern input then
                let matched_str : string = List.hd (Re.matches pattern input) in
                Some (tok, String.length matched_str)
            else None
        in
        match List.find_map tokenizer rules with 
            | Some (t, l) -> (t, l) 
            | None -> 
                let ident_regexp : Re.re = compile_start (Re.Posix.re "[A-Za-z][0-9A-Za-z]*") in
                if Re.execp ident_regexp input then
                    let ident = List.hd (Re.matches ident_regexp input) in 
                    (IDENT ident, String.length ident)
                else raise Lexer_error
    ) in
    [new_token] @ lexer (String.sub input consumed_len (String.length input - consumed_len))

open Why3;;

let parser (lexbuf: token list) (var_stack: Prove.stack_t) : Term.term =
    let exception Parser_error of string in

    let rec in_paren (tl: token list) : token list =
        match tl with
        | [] -> raise (Parser_error "Unclosed parenthese")
        | RPAREN :: _ -> []
        | token :: rest -> token :: (in_paren rest)
    in

    let rec list_skip (n: int) (l: 'a list) : 'a list = 
        let exception List_skip_exn in
        if n = 0 then l
        else match l with
        | [] -> raise List_skip_exn
        | _ :: l' -> list_skip (n - 1) l'
    in

    let rec parse_expr (lexbuf: token list) (ast: Term.term option) : Term.term option * int = 
        match lexbuf with
        | [] -> None, 0
        | EOL :: _ -> ast, 1
        | token :: rest ->
            let (new_ast, consumed) : Term.term option * int = 
                match token, ast with
                | TRUE, _ -> Some Term.t_true, 0
                | FALSE, _ -> Some Term.t_false, 0
                | IDENT name, _ -> Some (Prove.find_var name var_stack), 0
                | NOT, _ -> (
                    let rhs_opt, consumed = parse_expr rest None in
                    match rhs_opt with
                    | None -> raise (Parser_error "NOT : Incomplete assertion")
                    | Some rhs -> (Some (Term.t_not rhs), consumed + 1))
                | LPAREN, _ ->
                    let content = in_paren rest in 
                    fst (parse_expr content None), List.length content + 1
                | AND, Some ast_term -> (
                    let rhs_opt, consumed = parse_expr rest None in
                    match rhs_opt with
                    | None -> raise (Parser_error "AND : Incomplete assertion")
                    | Some rhs -> Some (Term.t_and ast_term rhs), consumed + 1)
                | OR, Some t -> (
                    let rhs_opt, consumed = parse_expr rest None in
                    match rhs_opt with
                    | None -> raise (Parser_error "OR : Incomplete assertion")
                    | Some rhs -> Some (Term.t_or t rhs), consumed + 1)
                | XOR, Some t -> (
                    let rhs_opt, consumed = parse_expr rest None in
                    match rhs_opt with
                    | None -> raise (Parser_error "XOR : Incomplete assertion")
                    | Some rhs -> Some (Prove.term_t_xor t rhs), consumed + 1)
                | EQV, Some t -> (
                    let rhs_opt, consumed = parse_expr rest None in
                    match rhs_opt with
                    | None -> raise (Parser_error "EQV : Incomplete assertion")
                    | Some rhs -> Some (Prove.term_t_eqv t rhs), consumed + 1)
                | _ -> raise (Parser_error "Error")
            in
            if consumed = List.length rest then
                new_ast, consumed
            else 
                parse_expr (list_skip consumed rest) new_ast

    in

    match fst (parse_expr lexbuf None) with
    | None -> raise (Parser_error "Empty Assertion")
    | Some result -> result

