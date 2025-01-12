%{
  open Why3
%}
%token TRUE FALSE
%token <string> IDENT
%token AND OR XOR EQV NOT
%token LPAREN RPAREN
%token EOL
%left AND OR XOR EQV    /* lowest precdence   */
%nonassoc NOT           /* highest precedence */
%start main
%type <Why3.Term.term> main
%%
main:
    expr EOL                { $1 }
;
expr:
      TRUE                  { Term.t_true }
    | FALSE                 { Term.t_false }
    | IDENT                 { Term.ps_app (Prove.find_var $1 stack)}
    | LPAREN expr RPAREN    { $2 }
    | expr AND expr         { Term.t_and $1 $3 }
    | expr OR expr          { Term.t_or $1 $3 }
    | expr XOR expr         { Prove.term_t_xor $1 $3 }
    | expr EQV expr         { Prover.term_t_eqv $1 $3 }
    | NOT expr              { Term.t_not $2 }