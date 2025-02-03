module IntMap = Map.Make(Int);;
module StrMap = Map.Make(String);;

type stack_t = Why3.Term.lsymbol StrMap.t;;
let find_var (name : string) (stack: stack_t) : Why3.Term.term = Why3.Term.ps_app (StrMap.find name stack) []
