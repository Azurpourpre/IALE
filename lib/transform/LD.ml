open Types.LD;;
open Export;;
exception Invalid_LD;;

let rec find_truth_value (component_id: int) (prgm: component Utils.IntMap.t) : eexpr =
  let rec component_input (parent_list: int list) : eexpr = 
    match parent_list with
    | [] -> EExpr.eFALSE
    | [pi] -> find_truth_value pi prgm
    | pi :: rest -> EExpr.orb (find_truth_value pi prgm) (component_input rest)
  in
  match Utils.IntMap.find component_id prgm with
  | LD_LEFT_POWERRAIL -> EExpr.eTRUE
  | LD_CONTACT {input = parent_list; variable = varname; negated = false} -> EExpr.andb (component_input parent_list) (EExpr.var varname)
  | LD_CONTACT {input = parent_list; variable = varname; negated = true} -> EExpr.andb (component_input parent_list) (EExpr.notb (EExpr.var varname))
  | LD_COIL {input = parent_list; variable = _; negated = false} -> component_input parent_list
  | LD_COIL {input = parent_list; variable = _; negated = true} -> component_input parent_list |> EExpr.notb
  | LD_RIGHT_POWERRAIL _ -> raise Invalid_LD 

let transform (prgm: component Utils.IntMap.t) : Export.eexpr list = 
  let coil_filter_func (_: int) (e: component) = 
    match e with
    | LD_COIL _ -> true
    | _ -> false
  in
  let coils_id : int list = Utils.IntMap.filter coil_filter_func prgm |> Utils.IntMap.to_list |> List.split |> fst in
  List.map (fun id -> find_truth_value id prgm) coils_id