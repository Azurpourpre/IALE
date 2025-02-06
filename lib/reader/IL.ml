open Types.IL;;
open M;;
exception Invalid_input;;
exception Invalid_label of string;;
exception Invalid_operator of string;;

let split (file_data: string) : (string option * string * string) list =
  let split_line (line:  string) : (string option * string * string) = 
    match List.filter (fun t -> String.length t > 0) (String.split_on_char ' ' line) with
    | [operator; operand] -> (None, operator, operand)
    | [label; operator; operand] -> (Some label, operator, operand)
    | _ -> raise Invalid_input
  in
  List.map split_line (String.split_on_char '\n' file_data)

let parser (token_list: (string option * string * string) list) : expr list = 
  let line_parser ((str_label, str_operator, str_operand): string option * string * string) : expr =
    let label : string option = match str_label with
      | None -> None
      | Some slabel -> if String.get slabel (String.length slabel - 1) = ':' then Some (String.sub slabel 0 (String.length slabel - 1)) else raise (Invalid_label slabel)
    in
    let operator : operator = match str_operator with
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
    let operand : operand = 
      match int_of_string_opt str_operand with
      | Some i -> INT i
      | None -> match float_of_string_opt str_operand with 
        | Some f -> REAL f
        | None -> match bool_of_string_opt str_operand with
          | Some b -> BOOL b
          | None -> VAR str_operand
    in (label, operator, operand)
  in
  List.map line_parser token_list

let read (file_data: Xml.xml) : expr list = 
  let code_section = follow file_data ["pou"; "body"; "IL"; "xhtml:p"] in
  match List.hd code_section with
  | PCData code -> split code |> parser
  | Element _ -> raise Invalid_input