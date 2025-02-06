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

val string_of_token : token -> string
val print_token_list : token list -> unit
val lexer : string -> token list
val parser : token list -> Utils.stack_t -> Why3.Term.term