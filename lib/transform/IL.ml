open Types.IL;;
open Utils;;
open Why3;;
exception Invalid_Real;;
exception Invalid_Operator of string;;
exception Invalid_Operand;;
exception Label_Not_Found;;

type result_type_t = 
| INT
| BOOL
| NONE
type result_t = {last_type: result_type_t ref; value: Term.term ref};;
let string_of_result_type (t: result_type_t) : string =
    match t with
    | INT -> "INT"
    | BOOL -> "BOOL"
    | NONE -> "NONE"

(* main code *)
let term_of_bool (b: bool) : Term.term = if b then Term.t_bool_true else Term.t_bool_false
let term_of_int (i: int) : Term.term = Term.t_int_const (BigInt.of_int i)
let transform_operand (e: operand) (stack: stack_t) : Term.term =
    match e with
    | INT i -> term_of_int i
    | BOOL b -> term_of_bool b
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
let result_type_from_operand (e: operand) (stack: stack_t) : result_type_t =
    match e with
    | INT _ -> INT
    | BOOL _ -> BOOL
    | VAR name -> 
        let vartype : Ty.ty = find_var name stack |> Term.t_type in
        if vartype = Ty.ty_int then INT
        else if vartype = Ty.ty_bool then BOOL
        else raise Invalid_Operand
    | REAL _ -> raise Invalid_Operand

let transform (prgm: expr list) (stack: stack_t) : Term.term =
    let do_N (n: bool) (arg: Term.term) : Term.term =
        if n then Term.t_not arg else arg
    in
    let transform_data_stmt ((_, op, arg): expr) (result: result_t) =
        let (new_type: result_type_t), (new_value: Term.term) = (
            match op, !(result.last_type) with
            | LD n, _ -> result_type_from_operand arg stack, transform_operand arg stack |> (do_N n)
            | AND n, BOOL -> BOOL, Prove.Bool.andb !(result.value) (transform_operand arg stack) |> do_N n
            | OR n, BOOL -> BOOL, Prove.Bool.orb !(result.value) (transform_operand arg stack) |> do_N n
            | XOR n, BOOL -> BOOL, Prove.Bool.xorb !(result.value) (transform_operand arg stack) |> do_N n
            | NOT, BOOL -> BOOL, Prove.Bool.notb !(result.value)
            | ADD, INT -> INT, Prove.Int.add !(result.value) (transform_operand arg stack)
            | SUB, INT -> INT, Prove.Int.sub !(result.value) (transform_operand arg stack)
            | MUL, INT -> INT, Prove.Int.mul !(result.value) (transform_operand arg stack)
            | DIV, INT -> INT, Prove.Int.div !(result.value) (transform_operand arg stack)
            | MOD, INT -> INT, Prove.Int.modulo !(result.value) (transform_operand arg stack)
            | EQ, INT -> BOOL, Prove.Int.eq !(result.value) (transform_operand arg stack)
            | NE, INT -> BOOL, Prove.Int.ne !(result.value) (transform_operand arg stack)
            | GT, INT -> BOOL, Prove.Int.gt !(result.value) (transform_operand arg stack)
            | GE, INT -> BOOL, Prove.Int.ge !(result.value) (transform_operand arg stack)
            | LT, INT -> BOOL, Prove.Int.lt !(result.value) (transform_operand arg stack)
            | LE, INT -> BOOL, Prove.Int.le !(result.value) (transform_operand arg stack)
            | _ -> raise (Invalid_Operator (string_of_operator op ^ " of " ^  string_of_result_type !(result.last_type)))
        ) in
        result.last_type := new_type; result.value := new_value
    in
    let get_arg_val (e: operand) : string = 
        match e with
        | VAR name -> name
        | _ -> raise Invalid_Operand
    in
    let get_arg_symbol (e: operand) : Term.vsymbol =
       StrMap.find (get_arg_val e) stack
    in
    let rec after_label (s: label) (l: expr list) : expr list = 
        match l with
        | [] -> raise Label_Not_Found
        | stmt :: rest -> 
            let lbl, _, _ = stmt in
            if lbl = Some s then l else after_label s rest
    in
    let result : result_t = {last_type = ref NONE; value = ref Term.t_false} in
    let rec transform_body (prgm: expr list) (stack: stack_t) : Term.term =
        match prgm with 
        | [] -> !(result.value)
        | stmt :: rest -> 
            let (_, op, arg) = stmt in
            match op with
            | ST n -> 
                let store_value = do_N n !(result.value) in
                result.value := find_var (get_arg_val arg) stack ;
                Term.t_let_close (get_arg_symbol arg) store_value (transform_body rest stack)
            | JMP (false, _) ->
                (* JMP *)
                transform_body (after_label (get_arg_val arg) rest) stack
            | JMP (true, n) -> 
                (* JMPC *)
                let cond_value = do_N n !(result.value) in
                Term.t_if cond_value (transform_body (after_label (get_arg_val arg) rest) stack) (transform_body rest stack)
            | _ -> transform_data_stmt stmt result; transform_body rest stack
    in
    transform_body prgm stack