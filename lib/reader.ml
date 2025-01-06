module IntMap = Map.Make(Int);;
type s12_variable = {
  name: string;
  address: string option;
  globalID: int option;
  vartype : Types.iectype;
  initial_value: string option;
};;
exception Invalid_input;;
exception Invalid_argument;;
exception Unexpected_value of string;;

let list_fold_cat (f: 'a -> 'b list) (l : 'a list) : 'b list =
  let fold_func = fun (acc : 'b list) n -> acc @ f n in
  List.fold_left fold_func [] l

let rec follow (xml_data : Xml.xml) (name_list : string list) : Xml.xml list =
  match xml_data with 
  | PCData _ -> raise Invalid_input
  | Element (name, _, child_list) -> 
    if List.hd name_list = name
    then
      if (List.length name_list = 1)
      then child_list
      else
        list_fold_cat (fun x -> follow x (List.tl name_list)) child_list
    else []

let parse_attr (attr_lst : (string * string) list) (attribute : string) : string option = 
  match List.find_opt (fun (k, _) -> k = attribute) attr_lst with
  | None -> None
  | Some (_, v) -> Some v

(* Variables reading & parsing *)
let parse_variable (xml_data : Xml.xml) : s12_variable option =
  let varfinder_func (data: Xml.xml) : Types.iectype option = 
    match data with
    | Element ("type", _, [Element (vartype, _, [])]) -> Types.iectype_of_string vartype
    | _ -> None
  in
  let initvalfinder_func (data : Xml.xml) : string option =
    match data with
    | Element ("initialValue", _, [Element ("simpleValue", valueattr, [])]) -> (parse_attr valueattr "value")
    | _ -> None
  in

  match xml_data with
  | PCData _ -> raise (Unexpected_value "PC Data found")
  | Element (variable, variable_attr, variable_child) -> 
    if variable <> "variable" then raise (Unexpected_value ("Expected variable. Got : '" ^ variable ^ "'"))
    else
      let (_, name) = List.find (fun (k,_) -> k = "name") variable_attr in
      let address = parse_attr variable_attr "address" in
      let globalID = (match parse_attr variable_attr "globalID" with | None -> None | Some s -> int_of_string_opt s) in 
      match List.find_map varfinder_func variable_child with 
      | None -> raise (Unexpected_value "Type can't be found") | Some vartype ->
      let initial_value = (List.find_map initvalfinder_func variable_child) in
      Some {name = name; address = address; globalID = globalID; vartype = vartype; initial_value = initial_value}

let read_variables (file_data: Xml.xml) : s12_variable list =
  let local_var_data = follow file_data ["project"; "types"; "pous"; "pou"; "interface"; "localVars"] in
  let fold_func (acc : s12_variable list) (s : Xml.xml) : s12_variable list =
    match parse_variable s with
    | Some d -> d :: acc
    | None -> acc
  in
  List.fold_left fold_func [] local_var_data

(* LD Reading & parsing *)

let rec read_LD_array (file_data: Xml.xml list) : Types.component_LD IntMap.t = 
  if List.is_empty file_data then IntMap.empty else
  let get_varname (xml_data: Xml.xml list) : string = 
    let varname_finder (xml_elm: Xml.xml) : bool = 
      match xml_elm with
      | Element ("variable", _, _) -> true
      | _ -> false
    in
    match List.find varname_finder xml_data with 
    | Element (_, _, [varname_xml]) ->(
      match varname_xml with 
      | Element _ -> raise Invalid_input
      | PCData s -> s )
    | _ -> raise (Unexpected_value "Wtf append in get_varname ???")
  in
  let get_conpred (xml_data: Xml.xml) : int list = 
    match xml_data with | PCData _ -> raise Invalid_input | Element (_,_, l1) -> 
      let cpi_data = List.find (fun (x: Xml.xml) -> match x with | Element ("connectionPointIn", _, _) -> true | _ -> false) l1 in
        match cpi_data with | PCData _ -> raise Invalid_input | Element (_,_, l2) ->
          let con_data_list = List.find_all (fun (x: Xml.xml) -> match x with | Element ("connection", _,_) -> true | _ -> false) l2 in
          list_fold_cat (fun (con_data: Xml.xml) -> 
            match con_data with 
            | PCData _ -> raise Invalid_input
            | Element (_,attr,_) -> match (parse_attr attr "refLocalId") with
              | None -> raise Invalid_input
              | Some s -> [int_of_string s]
            ) con_data_list
  in
  let eldata = List.hd file_data in
    match eldata with
    | PCData _ -> raise Invalid_input
    | Element (elname, elattr, elchildren) -> 
      let newel = (
        match elname with
        | "leftPowerRail" -> Some Types.LD_LEFT_POWERRAIL
        | "contact" -> Some (Types.LD_CONTACT  {input = get_conpred eldata; variable = get_varname elchildren; negated = false})
        | "rightPowerRail" -> Some (Types.LD_RIGHT_POWERRAIL (get_conpred eldata))
        | "coil" -> Some (Types.LD_COIL {input = get_conpred eldata; variable = get_varname elchildren; negated = false})
        | _ -> None
      ) in
    let newel_id = int_of_string ( match parse_attr elattr "localId" with | None -> raise Invalid_input | Some s -> s ) in
    match newel with
    | Some d -> IntMap.add newel_id d (read_LD_array (List.tl file_data))
    | None -> read_LD_array (List.tl file_data)

let read_LD (file_data: Xml.xml) : Types.component_LD Tree.t list =
  let rec build_tree (data: Types.component_LD IntMap.t) (rootID: int) : Types.component_LD Tree.t =
    let rootelm = IntMap.find rootID data in
    match rootelm with
    | Types.LD_RIGHT_POWERRAIL _ -> Leaf rootelm
    | _ -> Node (rootelm, IntMap.fold ( fun (key: int) (value: Types.component_LD) (acc: Types.component_LD Tree.t list) ->
      match value with
      | LD_LEFT_POWERRAIL -> acc
      | LD_RIGHT_POWERRAIL child_id  -> if List.mem rootID child_id then (build_tree data key) :: acc else acc
      | LD_CONTACT {input = child_id ; _} -> if List.mem rootID child_id then (build_tree data key) :: acc else acc
      | LD_COIL {input = child_id; _} -> if List.mem rootID child_id then (build_tree data key) :: acc else acc
    ) data [])
  in
  let ld_data = follow file_data ["project"; "types"; "pous"; "pou"; "body"; "LD"] in
  let ld_elm_arr = read_LD_array ld_data in 
  IntMap.fold (fun (key: int) (value: Types.component_LD) (acc: Types.component_LD Tree.t list) ->
    match value with
    | LD_LEFT_POWERRAIL -> (build_tree ld_elm_arr key) :: acc
    | _ -> acc
  ) ld_elm_arr []