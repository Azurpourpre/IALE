open IALE;;

let () = 
  print_endline "***   TEST LD   ***";
  let create_assertion (stack : Utils.stack_t) : Why3.Term.term =
      Why3.Term.t_iff (Utils.find_var "O" stack) (Why3.Term.t_and (Why3.Term.t_or (Utils.find_var "A" stack) (Utils.find_var "B" stack)) (Utils.find_var "C" stack))
  in

  let file_data = Xml.parse_file "hello_world.xml" in
  let program = Reader.M.get_program file_data "hello_world" in
  let ld_func : Types.LD.component Utils.IntMap.t = Reader.LD.read program in
  let stack : Utils.stack_t = Transform.Var.transform (Reader.Var.read program) in
  let logic : Why3.Term.term = Transform.LD.transform ld_func stack |> List.hd in
  let assertion : Why3.Term.term = create_assertion stack in
  let task : Why3.Task.task = Prove.create_task logic assertion stack in
  Prove.prove_with_any task