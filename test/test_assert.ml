open IALE;;

let test_lexer = 
  let exception Lexer_invalid in
  let input = "(true +~\tfalse) ::A0   #bVb" in
  let lexbuf = Assert.lexer input in
  if lexbuf <> [Assert.LPAREN; Assert.TRUE; Assert.OR; Assert.NOT; Assert.FALSE; Assert.RPAREN; Assert.EQV; Assert.IDENT "A0"; Assert.XOR; Assert.IDENT "bVb"; Assert.EOL]
  then raise Lexer_invalid
  else print_endline "Lexer valid !" 

let test_parser = 
  (* let exception Parser_invalid in *)
  let input : Assert.token list = [Assert.LPAREN; Assert.TRUE; Assert.OR; Assert.NOT; Assert.FALSE; Assert.RPAREN; Assert.EQV; Assert.IDENT "A0"; Assert.XOR; Assert.IDENT "bVb"; Assert.EOL] in
  let stack : Utils.stack_t = Utils.StrMap.of_list [("A0", (Why3.Term.create_psymbol (Why3.Ident.id_fresh "A0") [])); ("bVb", (Why3.Term.create_psymbol (Why3.Ident.id_fresh "bVb") []))] in
  let assert_term : Why3.Term.term = Assert.parser input stack in
  Format.printf "@[%a@]@." Why3.Pretty.print_term assert_term

let () =
  print_endline "***   TEST ASSERTION   ***";;
  test_lexer;;
  test_parser
  