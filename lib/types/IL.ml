type operator = 
    | LD of bool
    | ST of bool
    | S
    | R
    | AND of bool
    | OR of bool
    | XOR of bool
    | NOT
    | ADD
    | SUB
    | MUL
    | DIV
    | MOD
    | GT
    | GE
    | EQ
    | NE
    | LE
    | LT
    | JMP of (bool * bool) (* C modifier, N modifier*)

type operand =
    | INT of int
    | REAL of float
    | BOOL of bool
    | VAR of string

type label = string
type expr = label option * operator * operand

let string_of_operator (op: operator) : string =
    match op with
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