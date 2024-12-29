let format_var (var : IALE.Reader.s12_variable) : string =
  let name = var.name in
  let address = (match var.address with | Some s -> s | None -> "None") in
  let globalID = (match var.globalID with | Some i -> string_of_int i | None -> "None") in
  let vartype = IALE.Types.string_of_iectype var.vartype in
  let initval = (match var.initial_value with | Some s -> s | None -> "None") in
  "(name: " ^ name ^ ", address: " ^ address ^ ", globalID: " ^ globalID ^ ", vartype: " ^ vartype ^ ", initial value: " ^ initval ^ ")"

let rec print_varlist (l: IALE.Reader.s12_variable list) =
  print_endline (format_var (List.hd l));
  if List.length l > 1 then print_varlist (List.tl l)


let () =
  print_varlist (IALE.Reader.read_variables "plc.xml")