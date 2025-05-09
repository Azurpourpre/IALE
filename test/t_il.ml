open IALE;;

let create_assertion (collatz_func: Why3.Term.term) (stack: Utils.stack_t) : Why3.Term.term =
  let open Why3 in
  let an : Term.term = Utils.find_var "an" stack in
  Term.t_forall_close [Utils.StrMap.find "an" stack] [] (Prove.Int.ne collatz_func an)

let () = 
  print_endline "***   TEST IL   ***";
  let file_data = Xml.parse_file "hello_world.xml" in
  let program = Reader.M.get_program file_data "collatz" in
  let il_func : Types.IL.expr list = Reader.IL.read program in
  let stack : Utils.stack_t = Transform.Var.transform (Reader.Var.read program) in
  let why_func : Why3.Term.term = Transform.IL.transform il_func stack in
  let assertion : Why3.Term.term = create_assertion why_func stack in
  Prove.M.create_task assertion |> Prove.M.prove_with "Alt-Ergo";
  Prove.M.print_strats
