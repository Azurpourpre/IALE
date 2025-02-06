let () = 
  let vartype : IALE.Types.M.iectype = IALE.Types.M.USINT in
  print_endline "Hello, World!";
  if vartype = IALE.Types.M.USINT then print_endline "USINT" else print_endline "Other"
