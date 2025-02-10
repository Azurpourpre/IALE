open Why3;;

let config : Whyconf.config = Whyconf.init_config None
let main : Whyconf.main = Whyconf.get_main config
let env : Env.env = Env.create_env (Whyconf.loadpath main)
let bool_theory : Theory.theory = Env.read_theory env ["bool"] "Bool"

let notb (e: Term.term) : Term.term = 
  let not_func : Term.lsymbol = Theory.ns_find_ls bool_theory.th_export ["notb"] in
  Term.t_app_infer not_func [e]

let andb (a: Term.term) (b: Term.term) : Term.term = 
  let and_func : Term.lsymbol = Theory.ns_find_ls bool_theory.th_export ["andb"] in
  Term.t_app_infer and_func [a;b]

let orb (a: Term.term) (b: Term.term) : Term.term = 
  let or_func : Term.lsymbol = Theory.ns_find_ls bool_theory.th_export ["orb"] in
  Term.t_app_infer or_func [a;b]

let xorb (a: Term.term) (b: Term.term) : Term.term = 
  let xor_func : Term.lsymbol = Theory.ns_find_ls bool_theory.th_export ["xorb"] in
  Term.t_app_infer xor_func [a;b]

let eqvb (a: Term.term) (b: Term.term) : Term.term = 
  let eqv_func : Term.lsymbol = Theory.ns_find_ls bool_theory.th_export ["infix ="] in
  Term.t_app_infer eqv_func [a;b]

let use (t: Task.task) : Task.task =
  Task.use_export t bool_theory 