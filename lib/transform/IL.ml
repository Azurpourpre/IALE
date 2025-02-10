open Types.IL;;
open Utils;;
open Why3;;
exception Invalid_Real;;
exception Invalid_Operator of string;;

(* main code *)
let transform_operand (e: operand) (stack: stack_t) : Term.term =
    match e with
    | INT i -> Term.t_int_const (BigInt.of_int i)
    | BOOL b -> if b then Term.t_bool_true else Term.t_bool_false
    | VAR name -> find_var name stack
    | REAL f -> 
        print_endline "REAL variables are not yet supported !";
        if Float.is_finite f then 
            let pow5 : int = int_of_float (Float.div (Float.log f) (Float.log 5.0)) in 
            let f : float = Float.div f (Float.pow 5.0 (float_of_int pow5)) in
            let pow2 : int = int_of_float (Float.log2 f) in
            let n : int = int_of_float (Float.div f (Float.pow 2.0 (float_of_int pow2))) in
            Term.t_real_const ~pow2: (BigInt.of_int pow2) ~pow5: (BigInt.of_int pow5) (BigInt.of_int n)
        else raise Invalid_Real 

let transform_expr ((_, op, arg) : expr) (stack : stack_t) (result: Term.vsymbol) (rest: Term.term) : Term.term =

    let do_N (n: bool) (arg: Term.term) : Term.term = if n then Term.t_not arg else arg in
    let set_var (varname : Term.vsymbol) (value: Term.term) : Term.term = Term.t_let_close_simp varname value rest in
    let set_result : Term.term -> Term.term = set_var result in
    let get_result : Term.term = Term.t_var result in 

    match op with 
    | LD n -> transform_operand arg stack |> do_N n |> set_result
    | AND n -> Term.t_and get_result(transform_operand arg stack) |> do_N n |> set_result
    | OR n -> Term.t_or get_result(transform_operand arg stack) |> do_N n |> set_result
    | ADD -> Prove.Int.add get_result(transform_operand arg stack) |> set_result
    | SUB -> Prove.Int.sub get_result(transform_operand arg stack) |> set_result
    | MUL -> Prove.Int.mul get_result(transform_operand arg stack) |> set_result
    | DIV -> Prove.Int.div get_result(transform_operand arg stack) |> set_result
    | MOD -> Prove.Int.modulo get_result(transform_operand arg stack) |> set_result
    | _ -> raise (Invalid_Operator (Types.IL.string_of_operator op))
    

let transform (prgm: expr list) (stack: stack_t) : Term.term =
    let result : Term.vsymbol = Term.create_vsymbol (Ident.id_fresh "result__int") Ty.ty_int in
    let rec transform_body (prgm: expr list) (stack: stack_t) : Term.term =
        match prgm with
        | [] -> Term.t_var result
        | stmt :: rest -> transform_expr stmt stack result (transform_body rest stack)
    in
    transform_body prgm stack