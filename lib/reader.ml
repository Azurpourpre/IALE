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

let read_variables (filename: string) : s12_variable list =
  let file_data = Xml.parse_file filename in
  let local_var_data = follow file_data ["project"; "types"; "pous"; "pou"; "interface"; "localVars"] in
  let fold_func (acc : s12_variable list) (s : Xml.xml) : s12_variable list =
    match parse_variable s with
    | Some d -> d :: acc
    | None -> acc
  in
  List.fold_left fold_func [] local_var_data