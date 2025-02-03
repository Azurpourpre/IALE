open IALE;;
exception Error of string

let format_var (var : Reader.s12_variable) : string =
  let name = var.name in
  let address = (match var.address with | Some s -> s | None -> "None") in
  let globalID = (match var.globalID with | Some i -> string_of_int i | None -> "None") in
  let vartype = Types.string_of_iectype var.vartype in
  let initval = (match var.initial_value with | Some s -> s | None -> "None") in
  "(name: " ^ name ^ ", address: " ^ address ^ ", globalID: " ^ globalID ^ ", vartype: " ^ vartype ^ ", initial value: " ^ initval ^ ")"

let rec check_varlist (l: Reader.s12_variable list) =
  (match (List.hd l).name with
  | "O" -> if (format_var (List.hd l)) <> "(name: O, address: None, globalID: None, vartype: BOOL, initial value: None)" then raise (Error "Error in variable checking")
  | "A" -> if (format_var (List.hd l)) <>  "(name: A, address: None, globalID: None, vartype: BOOL, initial value: true)" then raise (Error "Error in variable checking")
  | "B" -> if (format_var (List.hd l)) <> "(name: B, address: None, globalID: None, vartype: BOOL, initial value: false)" then raise (Error "Error in variable checking")
  | "C" -> if (format_var (List.hd l)) <> "(name: C, address: None, globalID: None, vartype: BOOL, initial value: None)" then raise (Error "Error in variable checking")
  | _ -> raise (Error ("Too much variables: " ^ format_var (List.hd l)))
  );
  if List.length l > 1 then check_varlist (List.tl l)

let check_compmap (component_map : string Utils.IntMap.t) : unit =
  Utils.IntMap.iter (fun key value -> 
    match key with
    | 1 -> if value <> "LD_LEFT_POWERRAIL" then raise (Error "Invalid element 1 in check_compmap")
    | 6 -> if value <> "LD_CONTACT (variable : A, neg : false)" then raise (Error "Invalid element 6 in check_compmap")
    | 7 -> if value <> "LD_CONTACT (variable : B, neg : false)" then raise (Error "Invalid element 7 in check_compmap")
    | 8 -> if value <> "LD_CONTACT (variable : C, neg : false)" then raise (Error "Invalid element 8 in check_compmap")
    | 9 -> if value <> "LD_COIL (variable : O, neg : false)" then raise (Error "Invalid element 9 in check_compmap")
    | 10 -> if value <> "LD_RIGHT_POWERRAIL" then raise (Error "Invalid element 10 in check_compmap")
    | _ -> raise (Error "Invalid index in check_compmap")
    ) component_map

let () =
  print_endline "***   TEST XML   ***";;
  let file_data = Xml.parse_file "hello_world.xml" in
  Reader.read_variables file_data |> check_varlist ;print_endline "Variable corrects !";
  Utils.IntMap.map Types.format_LD (Reader.read_LD file_data) |> check_compmap; print_endline "LD Reading correct !"