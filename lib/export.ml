(* Helpers *)

let fold_cat (func: 'a -> string) (sep: string) (l: 'a list) =
    let strlist = List.fold_left (fun acc elm -> acc ^ (func elm) ^ sep) "" l in
    String.sub strlist 0 (String.length strlist - String.length sep)

(* End of Helpers *)


type etype = 
| INT
| BOOL
| REAL
| ARROW of etype * etype
| TUPLE of etype list

exception Invalid_IEC_Type_Export

let etype_from_iectype (t: Types.M.iectype) : etype =
    match t with
    | Types.M.BOOL -> BOOL
    | Types.M.BYTE | Types.M.WORD | Types.M.DWORD | Types.M.LWORD | Types.M.SINT | Types.M.INT | Types.M.DINT | Types.M.LINT | Types.M.USINT | Types.M.UINT | Types.M.UDINT | Types.M.ULINT | Types.M.ANY_INT -> INT
    | Types.M.REAL | Types.M.LREAL -> REAL
    | _ -> raise Invalid_IEC_Type_Export

let rec string_of_etype (e: etype) : string =
    match e with
    | INT -> "int"
    | BOOL -> "bool"
    | REAL -> "real"
    | ARROW (t1,t2) -> (string_of_etype t1) ^ "->" ^ (string_of_etype t2)
    | TUPLE l -> "(" ^ (fold_cat string_of_etype "," l) ^ ")"

type eexpr =
| CONST of etype * string (* value is always encoded as a string *)
| VAR of string
| AND of eexpr * eexpr
| OR of eexpr * eexpr
| NOT of eexpr
| IMPL of eexpr * eexpr
| EQV of eexpr * eexpr

module EExpr =
    struct
    let eFALSE = CONST (BOOL, "false")
    let eTRUE = CONST (BOOL, "true")
    let var (name: string) = VAR name

    let orb (v1: eexpr) (v2: eexpr) : eexpr = OR (v1, v2)
    let andb (v1: eexpr) (v2: eexpr) : eexpr = AND (v1, v2)
    let notb (v1: eexpr) : eexpr = NOT v1
    let impl (v1: eexpr) (v2: eexpr) = IMPL (v1, v2)
    let eqv (v1: eexpr) (v2: eexpr) = EQV (v1, v2)
end;;

type evar_hdr = etype * string (* variable is a type and a name*)

type efunc_hdr = evar_hdr list * etype * string (* a function is a list of variables (ie arguments), a return type and a name*)
type efunc = efunc_hdr * eexpr option * eexpr (* a function is a header, an optional spec and a body*)

let export_var (e: evar_hdr) : string =
    let t, name = e in
    Printf.sprintf "(%s : %s)" name (string_of_etype t)

let rec export_eexpr (e: eexpr) : string =
    match e with
    | CONST (_, v) -> v
    | VAR n -> n
    | AND (u,v) -> "(andb " ^ (export_eexpr u) ^ " " ^ (export_eexpr v) ^ ")"
    | OR (u,v) -> "(orb " ^ (export_eexpr u) ^ " " ^ (export_eexpr v) ^ ")"
    | NOT v -> "notb " ^ (export_eexpr v)
    | IMPL (u,v) -> (export_eexpr u) ^ " -> " ^ (export_eexpr v)
    | EQV (u,v) -> (export_eexpr u) ^ " <-> " ^ (export_eexpr v)

let export_function (f: efunc) : string =
    let (args, rettype, name), spec_o, body = f in
    match spec_o with
    | Some spec -> Printf.sprintf "let %s %s : %s ensures { result = %s } = %s" name (fold_cat export_var " " args) (string_of_etype rettype) (export_eexpr spec) (export_eexpr body)
    | None -> Printf.sprintf "let %s %s : %s = %s" name (fold_cat export_var " " args) (string_of_etype rettype) (export_eexpr body)


let export (filename: string) (f_list : efunc list) : unit = 
    let fp = open_out filename in
    let theory_name = String.split_on_char '.' filename |> List.hd in
    let functions_body = fold_cat export_function "\n    " f_list in
    Printf.fprintf fp "theory %s\n  use bool.Bool\n  %s\nend" theory_name functions_body