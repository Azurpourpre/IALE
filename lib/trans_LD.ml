open Utils;;
open Why3;;
exception Invalid_LD;;

let transform_ld (prgm: Types.LD.component IntMap.t) (stack: stack_t) : Term.term list = 
  let rec find_truth_value (component_id: int) (prgm: Types.LD.component IntMap.t) (stack: stack_t) : Term.term =
    let rec component_input (parent_list: int list) : Term.term = 
      match parent_list with
      | [] -> Term.t_false
      | [pi] -> find_truth_value pi prgm stack
      | pi :: rest -> Term.t_or (find_truth_value pi prgm stack) (component_input rest)
    in
    match IntMap.find component_id prgm with
    | LD_LEFT_POWERRAIL -> Term.t_true
    | LD_CONTACT {input = parent_list; variable = varname; negated = false} -> Term.t_and (component_input parent_list) (Utils.find_var varname stack)
    | LD_CONTACT {input = parent_list; variable = varname; negated = true} -> Term.t_and (component_input parent_list) (Term.t_not (Utils.find_var varname stack))
    | LD_COIL {input = parent_list; variable = _; negated = false} -> component_input parent_list
    | LD_COIL {input = parent_list; variable = _; negated = true} -> component_input parent_list |> Term.t_not
    | LD_RIGHT_POWERRAIL _ -> raise Invalid_LD 
  in
  let coils_id : int list = (IntMap.filter (fun _ e -> match e with | Types.LD.LD_COIL _ -> true | _ -> false) prgm |> IntMap.to_list |> List.split |> fst) in
  List.map (fun id -> find_truth_value id prgm stack) coils_id