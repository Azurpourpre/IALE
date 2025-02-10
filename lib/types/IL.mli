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

val string_of_operator : operator -> string