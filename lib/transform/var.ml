open Utils;;
open Why3;;
exception Not_Implemented of string;;

let transform_vartype (t: Types.M.iectype) : Ty.ty =
  match t with
  | BOOL -> Ty.ty_bool
  | BYTE
  | WORD
  | DWORD
  | LWORD
  | SINT
  | INT
  | DINT
  | LINT
  | USINT
  | UINT
  | UDINT
  | ULINT -> Ty.ty_int
  | REAL
  | LREAL -> Ty.ty_real
  | STRING
  | WSTRING -> Ty.ty_str

  | TIME
  | DATE
  | DT
  | TOD
  | ARRAY _
  | DERIVED _
  | ENUM _
  | SUBRANGESIGNED _
  | SUBRANGEUNSIGNED _
  | STRUCT _
  | ANY
  | ANY_DERIVED
  | ANY_ELEMENTARY
  | ANY_MAGNITUDE
  | ANY_NUM
  | ANY_REAL
  | ANY_INT
  | ANY_BIT
  | ANY_STRING
  | ANY_DATE
  | POINTER _ -> raise (Not_Implemented (Types.M.string_of_iectype t))

let transform (varlist: Reader.Var.t list) : stack_t = 
  let fold_func (acc: stack_t) (var: Reader.Var.t) : stack_t =
    StrMap.add var.name (Term.create_vsymbol (Ident.id_fresh var.name) (transform_vartype var.vartype)) acc
  in
  List.fold_left fold_func StrMap.empty varlist 