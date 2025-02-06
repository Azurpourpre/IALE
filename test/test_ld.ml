open IALE;;

let () = 
  print_endline "***   TEST LD   ***";
  let create_assertion (stack : Utils.stack_t) : Why3.Term.term =
      Why3.Term.t_iff (Utils.find_var "O" stack) (Why3.Term.t_and (Why3.Term.t_or (Utils.find_var "A" stack) (Utils.find_var "B" stack)) (Utils.find_var "C" stack))
  in

  let file_data = Xml.parse_file "hello_world.xml" in
  let program = Reader.get_program file_data "hello_world" in
  let ld_func : Types.LD.component Utils.IntMap.t = Reader.read_LD program in
  let stack : Utils.stack_t = Prove.transform_var (Reader.read_variables program) in
  let logic : Why3.Term.term = Prove.transform_ld ld_func stack |> List.hd in
  let assertion : Why3.Term.term = create_assertion stack in
  let task : Why3.Task.task = Prove.create_task logic assertion stack in
  Prove.prove_with_any task