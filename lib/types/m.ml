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
 
let category_of_type (t: iectype) : iectype_category = 
    match t with
    | BOOL | BYTE | WORD | DWORD | LWORD | SINT | INT | DINT | LINT | USINT | UINT | UDINT | ULINT | REAL | LREAL | TIME | DATE | DT | TOD | STRING | WSTRING -> ELEMENTARY
    | ARRAY _ | DERIVED _ | ENUM _ | SUBRANGESIGNED _ | SUBRANGEUNSIGNED _ | STRUCT _ -> DERIVED
    | ANY | ANY_DERIVED | ANY_ELEMENTARY | ANY_MAGNITUDE | ANY_NUM | ANY_REAL | ANY_INT | ANY_BIT | ANY_STRING | ANY_DATE -> GENERIC
    | POINTER _ -> EXTENDED

let iectype_of_string (s: string) : iectype option =
    match s with
    | "ANY" -> Some ANY
    | "ANY_DERIVED" -> Some ANY_DERIVED
    | "ANY_ELEMENTARY" -> Some ANY_ELEMENTARY
    | "ANY_MAGNITUDE" -> Some ANY_MAGNITUDE
    | "ANY_NUM" -> Some ANY_NUM
    | "ANY_REAL" -> Some ANY_REAL
    | "ANY_INT" -> Some ANY_INT
    | "ANY_BIT" -> Some ANY_BIT
    | "ANY_STRING" -> Some ANY_STRING
    | "ANY_DATE" -> Some ANY_DATE
    | "BOOL" -> Some BOOL
    | "BYTE" -> Some BYTE
    | "WORD" -> Some WORD
    | "DWORD" -> Some DWORD
    | "LWORD" -> Some LWORD
    | "SINT" -> Some SINT
    | "INT" -> Some INT
    | "DINT" -> Some DINT
    | "LINT" -> Some LINT
    | "USINT" -> Some USINT
    | "UINT" -> Some UINT
    | "UDINT" -> Some UDINT
    | "ULINT" -> Some ULINT
    | "REAL" -> Some REAL
    | "LREAL" -> Some LREAL
    | "TIME" -> Some TIME
    | "DATE" -> Some DATE
    | "DT" -> Some DT
    | "TOD" -> Some TOD
    | "STRING" -> Some STRING
    | "WSTRING" -> Some WSTRING
    | _ -> None

let string_of_iectype (t : iectype) : string = 
    match t with
    | BOOL -> "BOOL"
    | BYTE -> "BYTE"
    | WORD -> "WORD"
    | DWORD -> "DWORD"
    | LWORD -> "LWORD"
    | SINT -> "SINT"
    | INT -> "INT"
    | DINT -> "DINT"
    | LINT -> "LINT"
    | USINT -> "USINT"
    | UINT -> "UINT"
    | UDINT -> "UDINT"
    | ULINT -> "ULINT"
    | REAL -> "REAL"
    | LREAL -> "LREAL"
    | TIME -> "TIME"
    | DATE -> "DATE"
    | DT -> "DT"
    | TOD -> "TOD"
    | STRING -> "STRING"
    | WSTRING -> "WSTRING"
    | ARRAY _ -> "ARRAY"
    | DERIVED _ -> "DERIVED"
    | ENUM _ -> "ENUM"
    | SUBRANGESIGNED _ -> "SUBRANGESIGNED"
    | SUBRANGEUNSIGNED _ -> "SUBRANGEUNSIGNED"
    | STRUCT _ -> "STRUCT"
    | ANY -> "ANY"
    | ANY_DERIVED -> "ANY_DERIVED"
    | ANY_ELEMENTARY -> "ANY_ELEMENTARY"
    | ANY_MAGNITUDE -> "ANY_MAGNITUDE"
    | ANY_NUM -> "ANY_NUM"
    | ANY_REAL -> "ANY_REAL"
    | ANY_INT -> "ANY_INT"
    | ANY_BIT -> "ANY_BIT"
    | ANY_STRING -> "ANY_STRING"
    | ANY_DATE -> "ANY_DATE"
    | POINTER _ -> "POINTER"