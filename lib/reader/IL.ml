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
      | "LD" | "ld"          -> LD false
      | "LDN" | "ldn"         -> LD true
      | "ST" | "st"          -> ST false
      | "STN" | "stn"         -> ST true
      | "S" | "s"           -> S
      | "R" | "r"           -> R
      | "AND" | "and" | "&"   -> AND false
      | "ANDN" | "andn" | "&N" -> AND true 
      | "OR" | "or"          -> OR false
      | "ORN" | "orn"         -> OR true
      | "XOR" | "xor"         -> XOR false
      | "XORN" | "xorn"        -> XOR true
      | "NOT" | "not"         -> NOT
      | "ADD" | "add"         -> ADD
      | "SUB" | "sub"         -> SUB
      | "MUL" | "mul"         -> MUL
      | "DIV" | "div"         -> DIV
      | "MOD" | "mod"         -> MOD
      | "GT" | "gt"          -> GT
      | "GE" | "ge"          -> GE
      | "EQ" | "eq"          -> EQ
      | "NE" | "ne"          -> NE
      | "LE" | "le"          -> LE
      | "LT" | "lt"          -> LT
      | "JMP" | "jmp"         -> JMP (false, false)
      | "JMPC" | "jmpc"        -> JMP (true, false)
      | "JMPNC" | "jmpnc"       -> JMP (true, true)
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