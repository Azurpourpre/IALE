open M;;
open Types.M;;
type t = {
  name: string;
  address: string option;
  globalID: int option;
  vartype : iectype;
  initial_value: string option;
};;
exception Unexpected_value of string;;

let parse (xml_data : Xml.xml) : t option =
  let varfinder_func (data: Xml.xml) : iectype option = 
    match data with
    | Element ("type", _, [Element (vartype, _, [])]) -> iectype_of_string vartype
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
      let address = M.parse_attr variable_attr "address" in
      let globalID = (match M.parse_attr variable_attr "globalID" with | None -> None | Some s -> int_of_string_opt s) in 
      match List.find_map varfinder_func variable_child with 
      | None -> raise (Unexpected_value "Type can't be found") | Some vartype ->
      let initial_value = (List.find_map initvalfinder_func variable_child) in
      Some {name = name; address = address; globalID = globalID; vartype = vartype; initial_value = initial_value}

let read (file_data: Xml.xml) : t list =
  let local_var_data = follow file_data ["pou"; "interface"; "localVars"] in
  let fold_func (acc : t list) (s : Xml.xml) : t list =
    match parse s with
    | Some d -> d :: acc
    | None -> acc
  in
  List.fold_left fold_func [] local_var_data