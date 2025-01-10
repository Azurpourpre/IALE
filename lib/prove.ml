module StrMap = Map.Make(String);;
open Why3;;
exception Invalid_LD;;
exception Invalid_var of string;;

let term_t_eqv (a: Term.term) (b: Term.term) : Term.term =
  Term.t_or (Term.t_and a b) (Term.t_and (Term.t_not a) (Term.t_not b))

let term_t_xor (a: Term.term) (b: Term.term) : Term.term = 
  Term.t_and (Term.t_or a b) (Term.t_not (Term.t_and a b))

let transform_var (varlist: Reader.s12_variable list) : Term.lsymbol StrMap.t = 
  let fold_func (acc: Term.lsymbol StrMap.t) (var: Reader.s12_variable) : Term.lsymbol StrMap.t =
    acc |> StrMap.add var.name (Term.create_psymbol (Ident.id_fresh var.name) [])
  in
  List.fold_left fold_func StrMap.empty varlist 

let rec transform_ld (prgm : Types.component_LD Tree.t) (stack : Term.lsymbol StrMap.t) : Term.term =
  let recurse_children (children : Types.component_LD Tree.t list) : Term.term = Term.t_or_l (List.map (fun e -> transform_ld e stack) children) in
  match prgm with
  | Node (Types.LD_LEFT_POWERRAIL, children) -> recurse_children children
  | Node (Types.LD_CONTACT {input = _; variable = varname; negated = _}, children) -> Term.t_and (Term.ps_app (StrMap.find varname stack) []) (recurse_children children)
  | Node (Types.LD_COIL {input = _; variable = varname; negated = _}, children) -> term_t_xor (Term.ps_app (StrMap.find varname stack) []) (recurse_children children)
  | Leaf Types.LD_RIGHT_POWERRAIL _ -> Term.t_false
  | _ -> raise Invalid_LD

let create_task (prgm: Term.term) (assertion : Term.term) (stack : Term.lsymbol StrMap.t) = 
  let task = StrMap.fold (fun _ symbol acc_task : Task.task -> Task.add_param_decl acc_task symbol) stack None in
  let goal : Decl.prsymbol = Decl.create_prsymbol (Ident.id_fresh "Goal1") in
  Task.add_prop_decl task Decl.Pgoal goal (Term.t_implies prgm assertion)

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
  
