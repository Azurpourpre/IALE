let () = 
  let vartype : IALE.Types.iectype = IALE.Types.USINT in
  print_endline "Hello, World!";
  if vartype = IALE.Types.USINT then print_endline "USINT" else print_endline "Other"
