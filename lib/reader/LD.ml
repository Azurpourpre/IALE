open Types.LD;;
open Utils;;
open M;;
exception Invalid_input;;
exception Unexpected_value of string;;

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

let rec read_body (file_data: Xml.xml list) : component IntMap.t =
  if List.is_empty file_data then IntMap.empty else
  let eldata = List.hd file_data in
    match eldata with
    | PCData _ -> raise Invalid_input
    | Element (elname, elattr, elchildren) -> 
      let newel = (
        match elname with
        | "leftPowerRail" -> Some LD_LEFT_POWERRAIL
        | "contact" -> Some (LD_CONTACT  {input = get_conpred eldata; variable = get_varname elchildren; negated = false})
        | "rightPowerRail" -> Some (LD_RIGHT_POWERRAIL (get_conpred eldata))
        | "coil" -> Some (LD_COIL {input = get_conpred eldata; variable = get_varname elchildren; negated = false})
        | _ -> None
      ) in
    let newel_id = int_of_string ( match parse_attr elattr "localId" with | None -> raise Invalid_input | Some s -> s ) in
    match newel with
    | Some d -> IntMap.add newel_id d (read_body (List.tl file_data))
    | None -> read_body (List.tl file_data)
let read (file_data: Xml.xml) : component IntMap.t =
  read_body (follow file_data ["pou"; "body"; "LD"])