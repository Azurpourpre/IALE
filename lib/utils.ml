module IntMap = Map.Make(Int);;
module StrMap = Map.Make(String);;

type stack_t = Why3.Term.vsymbol StrMap.t;;
let find_var (name : string) (stack: stack_t) : Why3.Term.term = Why3.Term.t_var (StrMap.find name stack)

let print_task (t : Why3.Task.task) =
  Format.printf "@[@\n%a@]@." Why3.Pretty.print_task t