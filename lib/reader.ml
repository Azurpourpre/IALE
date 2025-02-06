open Utils;;
open Types;;
type s12_variable = {
  name: string;
  address: string option;
  globalID: int option;
  vartype : iectype;
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


let get_program (xml_data: Xml.xml) (name: string) : Xml.xml =
  let filter_func (pou: Xml.xml) : bool =
    match pou with
    | Element ("pou", attr, _) -> parse_attr attr "name" = Some name
    | _ -> false
  in
  follow xml_data ["project"; "types"; "pous"] |> List.find filter_func 

(* Variables reading & parsing *)
let parse_variable (xml_data : Xml.xml) : s12_variable option =
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
      let address = parse_attr variable_attr "address" in
      let globalID = (match parse_attr variable_attr "globalID" with | None -> None | Some s -> int_of_string_opt s) in 
      match List.find_map varfinder_func variable_child with 
      | None -> raise (Unexpected_value "Type can't be found") | Some vartype ->
      let initial_value = (List.find_map initvalfinder_func variable_child) in
      Some {name = name; address = address; globalID = globalID; vartype = vartype; initial_value = initial_value}

let read_variables (file_data: Xml.xml) : s12_variable list =
  let local_var_data = follow file_data ["pou"; "interface"; "localVars"] in
  let fold_func (acc : s12_variable list) (s : Xml.xml) : s12_variable list =
    match parse_variable s with
    | Some d -> d :: acc
    | None -> acc
  in
  List.fold_left fold_func [] local_var_data

(* LD Reading & parsing *)

let read_LD (file_data: Xml.xml) : LD.component IntMap.t =

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

  let rec read_LD_body (file_data: Xml.xml list) : LD.component IntMap.t =
    if List.is_empty file_data then IntMap.empty else
    let eldata = List.hd file_data in
      match eldata with
      | PCData _ -> raise Invalid_input
      | Element (elname, elattr, elchildren) -> 
        let newel = (
          match elname with
          | "leftPowerRail" -> Some LD.LD_LEFT_POWERRAIL
          | "contact" -> Some (LD.LD_CONTACT  {input = get_conpred eldata; variable = get_varname elchildren; negated = false})
          | "rightPowerRail" -> Some (LD.LD_RIGHT_POWERRAIL (get_conpred eldata))
          | "coil" -> Some (LD.LD_COIL {input = get_conpred eldata; variable = get_varname elchildren; negated = false})
          | _ -> None
        ) in
      let newel_id = int_of_string ( match parse_attr elattr "localId" with | None -> raise Invalid_input | Some s -> s ) in
      match newel with
      | Some d -> IntMap.add newel_id d (read_LD_body (List.tl file_data))
      | None -> read_LD_body (List.tl file_data)
  in

  read_LD_body (follow file_data ["pou"; "body"; "LD"])

(* IL Reading & Parsing *)

exception Invalid_label of string;;
exception Invalid_operator of string;;

let splitter_IL (file_data: string) : (string option * string * string) list =
  let split_line (line:  string) : (string option * string * string) = 
    match List.filter (fun t -> String.length t > 0) (String.split_on_char ' ' line) with
    | [operator; operand] -> (None, operator, operand)
    | [label; operator; operand] -> (Some label, operator, operand)
    | _ -> raise Invalid_input
  in
  List.map split_line (String.split_on_char '\n' file_data)

let parser_IL (token_list: (string option * string * string) list) : IL.expr list = 
  let line_parser ((str_label, str_operator, str_operand): string option * string * string) : IL.expr =
    let label : string option = match str_label with
      | None -> None
      | Some slabel -> if String.get slabel (String.length slabel - 1) = ':' then Some (String.sub slabel 0 (String.length slabel - 1)) else raise (Invalid_label slabel)
    in
    let operator : IL.operator = match str_operator with
      | "LD"          -> LD false
      | "LDN"         -> LD true
      | "ST"          -> ST false
      | "STN"         -> ST true
      | "S"           -> S
      | "R"           -> R
      | "AND" | "&"   -> AND false
      | "ANDN" | "&N" -> AND true 
      | "OR"          -> OR false
      | "ORN"         -> OR true
      | "XOR"         -> XOR false
      | "XORN"        -> XOR true
      | "NOT"         -> NOT
      | "ADD"         -> ADD
      | "SUB"         -> SUB
      | "MUL"         -> MUL
      | "DIV"         -> DIV
      | "MOD"         -> MOD
      | "GT"          -> GT
      | "GE"          -> GE
      | "EQ"          -> EQ
      | "NE"          -> NE
      | "LE"          -> LE
      | "LT"          -> LT
      | "JMP"         -> JMP (false, false)
      | "JMPC"        -> JMP (true, false)
      | "JMPNC"       -> JMP (true, true)
      | _ -> raise (Invalid_operator str_operator)
    in
    let operand : IL.operand = 
      match int_of_string_opt str_operand with
      | Some i -> IL.INT i
      | None -> match float_of_string_opt str_operand with 
        | Some f -> IL.REAL f
        | None -> match bool_of_string_opt str_operand with
          | Some b -> IL.BOOL b
          | None -> IL.VAR str_operand
    in (label, operator, operand)
  in
  List.map line_parser token_list

let read_IL (file_data: Xml.xml) : IL.expr list = 
  let code_section = follow file_data ["pou"; "body"; "IL"; "xhtml:p"] in
  match List.hd code_section with
  | PCData code -> splitter_IL code |> parser_IL
  | Element _ -> raise Invalid_input