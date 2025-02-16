open Why3;;

let config : Whyconf.config = Whyconf.init_config None
let main : Whyconf.main = Whyconf.get_main config
let env : Env.env = Env.create_env (Whyconf.loadpath main)
let int_theory : Theory.theory = Env.read_theory env ["int"] "Int"
let euclideandiv_theory : Theory.theory = Env.read_theory env ["int"] "EuclideanDivision"


let use (t : Task.task) : Task.task =
  Task.use_export t int_theory

let plus_symbol : Term.lsymbol = Theory.ns_find_ls int_theory.th_export ["infix +"]
let add (a: Term.term) (b: Term.term) : Term.term = 
  Term.t_app_infer plus_symbol [a;b]

let minus_symbol : Term.lsymbol = Theory.ns_find_ls int_theory.th_export ["infix -"]
let sub (a: Term.term) (b: Term.term) : Term.term = 
  Term.t_app_infer minus_symbol [a;b]

let mul_symbol : Term.lsymbol = Theory.ns_find_ls int_theory.th_export ["infix *"]
let mul (a: Term.term) (b: Term.term) : Term.term = 
  Term.t_app_infer mul_symbol [a;b]

let div_func : Term.lsymbol = Theory.ns_find_ls euclideandiv_theory.th_export ["div"]
let div (a: Term.term) (b: Term.term) : Term.term = 
  Term.t_app_infer div_func [a;b]

let mod_func : Term.lsymbol = Theory.ns_find_ls euclideandiv_theory.th_export ["mod"]
let modulo (a: Term.term) (b: Term.term) : Term.term = 
  Term.t_app_infer mod_func [a;b]

let gt_func : Term.lsymbol = Theory.ns_find_ls int_theory.th_export ["infix >"]
let gt (a: Term.term) (b: Term.term) : Term.term = 
  Term.t_app_infer gt_func [a;b]

let ge_func : Term.lsymbol = Theory.ns_find_ls int_theory.th_export ["infix >="]
let ge (a: Term.term) (b: Term.term) : Term.term = 
  Term.t_app_infer ge_func [a;b]

let le_func : Term.lsymbol = Theory.ns_find_ls int_theory.th_export ["infix <="]
let le (a: Term.term) (b: Term.term) : Term.term = 
  Term.t_app_infer le_func [a;b]

let lt_func : Term.lsymbol = Theory.ns_find_ls int_theory.th_export ["infix <"]
let lt (a: Term.term) (b: Term.term) : Term.term = 
  Term.t_app_infer lt_func [a;b]

let eq = Term.t_equ
let ne = Term.t_neq