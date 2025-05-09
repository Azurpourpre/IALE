open IALE.Export;;

let test_type = TUPLE [INT; BOOL; ARROW (INT,REAL)]

let test_func_hdr : efunc_hdr = ([(INT, "n"); (BOOL, "b"); (test_type, "complex_var")], INT, "function_test")
let test_func2_hdr : efunc_hdr = ([REAL, "r"], REAL, "test2")


let () =
  export "export.mlw" [(test_func_hdr, None, VAR "test function"); (test_func2_hdr, Some EExpr.eFALSE, CONST (TUPLE [], "()"))];