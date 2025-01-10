let () = 
  let create_assertion (stack : Why3.Term.lsymbol IALE.Prove.StrMap.t) : Why3.Term.term =
    let find_var (name : string) : Why3.Term.term = Why3.Term.ps_app (IALE.Prove.StrMap.find name stack) [] in
      Why3.Term.t_iff (find_var "O") (Why3.Term.t_and (Why3.Term.t_or (find_var "A") (find_var "B")) (find_var "C"))
  in

  let file_data = Xml.parse_file "hello_world.xml" in
  let ld_func : IALE.Types.component_LD IALE.Tree.t = List.hd (IALE.Reader.read_LD file_data) in
  let stack : Why3.Term.lsymbol IALE.Prove.StrMap.t = IALE.Prove.transform_var (IALE.Reader.read_variables file_data) in
  let logic : Why3.Term.term = IALE.Prove.transform_ld ld_func stack in
  let assertion : Why3.Term.term = create_assertion stack in
  let task : Why3.Task.task = IALE.Prove.create_task logic assertion stack in
  IALE.Prove.prove_with_any task