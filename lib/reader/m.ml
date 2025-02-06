exception Invalid_input;;

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

let get_program (xml_data: Xml.xml) (name: string) : Xml.xml =
  let filter_func (pou: Xml.xml) : bool =
    match pou with
    | Element ("pou", attr, _) -> parse_attr attr "name" = Some name
    | _ -> false
  in
  follow xml_data ["project"; "types"; "pous"] |> List.find filter_func 