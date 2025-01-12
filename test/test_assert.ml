let test_lexer = 
  let exception Lexer_invalid in
  let input = "(true +~\tfalse) ::A0   #bVb" in
  let lexbuf = IALE.Assert.lexer input in
  if lexbuf <> [IALE.Assert.LPAREN; IALE.Assert.TRUE; IALE.Assert.OR; IALE.Assert.NOT; IALE.Assert.FALSE; IALE.Assert.RPAREN; IALE.Assert.EQV; IALE.Assert.IDENT "A0"; IALE.Assert.XOR; IALE.Assert.IDENT "bVb"; IALE.Assert.EOL]
  then raise Lexer_invalid
  else print_endline "Lexer valid !" 

let test_parser = 
  (* let exception Parser_invalid in *)
  let input : IALE.Assert.token list = [IALE.Assert.LPAREN; IALE.Assert.TRUE; IALE.Assert.OR; IALE.Assert.NOT; IALE.Assert.FALSE; IALE.Assert.RPAREN; IALE.Assert.EQV; IALE.Assert.IDENT "A0"; IALE.Assert.XOR; IALE.Assert.IDENT "bVb"; IALE.Assert.EOL] in
  let stack : IALE.Prove.stack_t = IALE.Prove.StrMap.of_list [("A0", (Why3.Term.create_psymbol (Why3.Ident.id_fresh "A0") [])); ("bVb", (Why3.Term.create_psymbol (Why3.Ident.id_fresh "bVb") []))] in
  let assert_term : Why3.Term.term = IALE.Assert.parser input stack in
  Format.printf "@[%a@]@." Why3.Pretty.print_term assert_term

let () =
  print_endline "***   TEST ASSERTION   ***";
  test_lexer;
  test_parser
  