exception Error of string

let format_var (var : IALE.Reader.s12_variable) : string =
  let name = var.name in
  let address = (match var.address with | Some s -> s | None -> "None") in
  let globalID = (match var.globalID with | Some i -> string_of_int i | None -> "None") in
  let vartype = IALE.Types.string_of_iectype var.vartype in
  let initval = (match var.initial_value with | Some s -> s | None -> "None") in
  "(name: " ^ name ^ ", address: " ^ address ^ ", globalID: " ^ globalID ^ ", vartype: " ^ vartype ^ ", initial value: " ^ initval ^ ")"

let rec check_varlist (l: IALE.Reader.s12_variable list) =
  (match (List.hd l).name with
  | "o1" -> if (format_var (List.hd l)) <> "(name: o1, address: None, globalID: None, vartype: BOOL, initial value: None)" then raise (Error "Error in variable checking")
  | "i1" -> if (format_var (List.hd l)) <>  "(name: i1, address: None, globalID: None, vartype: BOOL, initial value: true)" then raise (Error "Error in variable checking")
  | "i2" -> if (format_var (List.hd l)) <> "(name: i2, address: None, globalID: None, vartype: BOOL, initial value: false)" then raise (Error "Error in variable checking")
  | "i3" -> if (format_var (List.hd l)) <> "(name: i3, address: None, globalID: None, vartype: BOOL, initial value: None)" then raise (Error "Error in variable checking")
  | _ -> raise (Error ("Too much variables: " ^ format_var (List.hd l)))
  );
  if List.length l > 1 then check_varlist (List.tl l)

let rec print_components (l: IALE.Types.component_LD IALE.Reader.tree) (indent: int) = 
  let rec stringn (s: string) (n: int) = 
    if n = 0 then "" else (stringn s (n-1)) ^ s
  in
  match l with
  | Leaf e -> print_endline ((stringn "\t" indent) ^ (IALE.Types.format_LD e)) 
  | Node (e, childs) -> 
    print_endline ((stringn "\t" indent) ^ (IALE.Types.format_LD e));
    List.iter (fun c -> print_components c (indent + 1)) childs


let () =
  let file_data = Xml.parse_file "hello_world.xml" in (
  check_varlist (IALE.Reader.read_variables file_data); print_endline "Variable corrects !";
  print_endline "\n\t***   LD Components   ***\n";
  List.iter (fun (t: IALE.Types.component_LD IALE.Reader.tree) -> print_components t 0) (IALE.Reader.read_LD file_data))