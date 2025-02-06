open IALE;;

let format_expr ((label_o, operator, operand) : Types.IL.expr) : string = 
  let slabel : string = (
    match label_o with 
    | Some s -> ("label : " ^ s ^ " ; ")
    | None -> ""
  ) in
  let soperator : string = (
    match operator with
    | LD b -> if b then "LDN" else "LD"
    | ST b -> if b then "STN" else "ST"
    | S -> "S"
    | R -> "R"
    | AND b -> if b then "ANDN" else "AND"
    | OR b -> if b then "ORN" else "OR"
    | XOR b -> if b then "XORN" else "XOR"
    | NOT -> "NOT"
    | ADD -> "ADD"
    | SUB -> "SUB"
    | MUL -> "MUL"
    | DIV -> "DIV"
    | MOD -> "MOD"
    | GT ->  "GT"
    | GE ->  "GE"
    | EQ ->  "EQ"
    | NE ->  "NE"
    | LE ->  "LE"
    | LT ->  "LT"
    | JMP (cmod, nmod) -> if nmod then "JMPNC" else if cmod then "JMPC" else "JMP"

  ) in
  let soperand : string = (
    match operand with
    | INT i -> "INT{" ^ (string_of_int i) ^ "}"
    | REAL f -> "REAL{" ^ (string_of_float f) ^ "}"
    | BOOL b -> "BOOL{" ^ (string_of_bool b) ^ "}"
    | VAR name -> "VAR{" ^ name ^ "}"
  ) in
  (slabel ^ "operator : " ^ soperator ^ " ; operand : " ^ soperand)

let () = 
  print_endline "***   TEST IL   ***";
  let file_data = Xml.parse_file "hello_world.xml" in
  let program = Reader.get_program file_data "collatz" in
  let il_func : Types.IL.expr list = Reader.read_IL program in
  List.iter (fun e -> print_endline (format_expr e)) il_func