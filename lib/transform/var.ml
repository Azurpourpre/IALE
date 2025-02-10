open Utils;;
open Why3;;

let transform (varlist: Reader.Var.t list) : stack_t = 
  let fold_func (acc: stack_t) (var: Reader.Var.t) : stack_t =
    acc |> StrMap.add var.name (Term.create_psymbol (Ident.id_fresh var.name) [])
  in
  List.fold_left fold_func StrMap.empty varlist 