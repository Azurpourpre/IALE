open Why3;;

let config : Whyconf.config = Whyconf.init_config None
let main : Whyconf.main = Whyconf.get_main config
let env : Env.env = Env.create_env (Whyconf.loadpath main)
let int_theory : Theory.theory = Env.read_theory env ["int"] "Int"

let add (a: Term.term) (b: Term.term) : Term.term =
  let plus_symbol : Term.lsymbol = Theory.ns_find_ls int_theory.th_export ["infix +"] in
  Term.t_app_infer plus_symbol [a;b]

let sub (a: Term.term) (b: Term.term) : Term.term =
  let minus_symbol : Term.lsymbol = Theory.ns_find_ls int_theory.th_export ["infix -"] in
  Term.t_app_infer minus_symbol [a;b]

let mul (a: Term.term) (b: Term.term) : Term.term =
  let mul_symbol : Term.lsymbol = Theory.ns_find_ls int_theory.th_export ["infix *"] in
  Term.t_app_infer mul_symbol [a;b]

let div (a: Term.term) (b: Term.term) : Term.term = 
    (* Should check if division returns a real or an int (and which int ??)*)
  let div_func : Term.lsymbol = Theory.ns_find_ls int_theory.th_export ["div"] in
  Term.t_app_infer div_func [a;b]

let modulo (a: Term.term) (b: Term.term) : Term.term = 
  let mod_func : Term.lsymbol = Theory.ns_find_ls int_theory.th_export ["mod"] in
  Term.t_app_infer mod_func [a;b]