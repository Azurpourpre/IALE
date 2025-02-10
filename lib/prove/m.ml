open Why3;;

let create_task (prgm: Term.term) (assertion : Term.term) (stack: Utils.stack_t): Task.task = 
  let blank_task = Bool.use None |> Int.use in
  let vsymbol_list : Term.vsymbol list = List.map snd (Utils.StrMap.to_list stack) in
  let goal_term : Term.term = Term.t_forall_close_simp vsymbol_list [] (Term.t_implies prgm assertion) in
  let goal : Decl.prsymbol = Decl.create_prsymbol (Ident.id_fresh "Goal1") in 
  Task.add_prop_decl blank_task Decl.Pgoal goal goal_term

let prove_with_any (task: Task.task) = 
  let config : Whyconf.config = Whyconf.init_config None in
  let main : Whyconf.main = Whyconf.get_main config in
  let limits = Call_provers.{empty_limits with limit_time = Whyconf.timelimit main; limit_mem = Whyconf.memlimit main} in
  let provers : Whyconf.config_prover Whyconf.Mprover.t = Whyconf.get_provers config in
  let prover_config = snd (Whyconf.Mprover.max_binding provers) in
  let env : Env.env = Env.create_env (Whyconf.loadpath main) in
  let driver : Driver.driver = Driver.load_driver_for_prover main env prover_config in
  let result : Call_provers.prover_result = Call_provers.wait_on_call (Driver.prove_task ~limits ~config:main ~command:prover_config.command driver task) in
  Format.printf "@[On task, solver answers %a@]@." (Call_provers.print_prover_result ?json:None) result
  
