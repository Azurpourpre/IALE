let () = 
  print_endline "***   TEST LD   ***";
  let create_assertion (stack : IALE.Prove.stack_t) : Why3.Term.term =
      Why3.Term.t_iff (IALE.Prove.find_var "O" stack) (Why3.Term.t_and (Why3.Term.t_or (IALE.Prove.find_var "A" stack) (IALE.Prove.find_var "B" stack)) (IALE.Prove.find_var "C" stack))
  in

  let file_data = Xml.parse_file "hello_world.xml" in
  let ld_func : IALE.Types.component_LD IALE.Tree.t = List.hd (IALE.Reader.read_LD file_data) in
  let stack : IALE.Prove.stack_t = IALE.Prove.transform_var (IALE.Reader.read_variables file_data) in
  let logic : Why3.Term.term = IALE.Prove.transform_ld ld_func stack in
  let assertion : Why3.Term.term = create_assertion stack in
  let task : Why3.Task.task = IALE.Prove.create_task logic assertion stack in
  IALE.Prove.prove_with_any task