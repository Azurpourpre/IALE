open IALE;;

let declare_var (name_l : string list) : Export.evar_hdr list =
  List.map (fun name -> (Export.BOOL, name)) name_l

let () = 
  print_endline "***   TEST LD   ***";

  let file_data = Xml.parse_file "hello_world.xml" in
  let program = Reader.M.get_program file_data "hello_world" in
  let ld_func : Types.LD.component Utils.IntMap.t = Reader.LD.read program in
  let assertion : Export.eexpr = (Export.EExpr.andb (Export.EExpr.orb (Export.EExpr.var "a") (Export.EExpr.var "b")) (Export.EExpr.var "c")) in
  let logic : Export.efunc = ((declare_var ["a"; "b"; "c"], Export.BOOL, "extraction"), Some assertion, Transform.LD.transform ld_func |> List.hd) in
  Export.export "LD_test.mlw" [logic]