module IntMap = Map.Make(Int);;
module StrMap = Map.Make(String);;
exception Variable_Not_Found;;

type stack_t = Why3.Term.vsymbol StrMap.t;;
let find_var (name : string) (stack: stack_t) : Why3.Term.term = 
  match StrMap.find_opt name stack with
  | Some symbol -> Why3.Term.t_var symbol
  | None -> raise Variable_Not_Found

let print_task (t : Why3.Task.task) =
  Format.printf "@[@\n%a@]@." Why3.Pretty.print_task t