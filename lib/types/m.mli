type iectype = 
    | BOOL
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
    | ULINT
    | REAL
    | LREAL
    | TIME
    | DATE
    | DT
    | TOD
    | STRING
    | WSTRING
    | ARRAY of iectype
    | DERIVED of iectype
    | ENUM of iectype
    | SUBRANGESIGNED of (int * int)
    | SUBRANGEUNSIGNED of (int * int)
    | STRUCT of iectype list
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
    | POINTER of iectype

type iectype_category = 
    | ELEMENTARY
    | DERIVED
    | GENERIC
    | EXTENDED

val category_of_type : iectype -> iectype_category
val iectype_of_string : string -> iectype option
val string_of_iectype : iectype -> string