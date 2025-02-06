type component = 
  | LD_LEFT_POWERRAIL
  | LD_RIGHT_POWERRAIL of int list
  | LD_CONTACT of {input: int list; variable: string; negated: bool;}
  | LD_COIL of {input: int list; variable : string; negated : bool;}

let format (cmp : component) : string = 
    match cmp with
    | LD_LEFT_POWERRAIL -> "LD_LEFT_POWERRAIL"
    | LD_RIGHT_POWERRAIL _ -> "LD_RIGHT_POWERRAIL"
    | LD_CONTACT {input = _; variable = varname; negated = negval} -> "LD_CONTACT (variable : " ^ varname ^ ", neg : " ^ (string_of_bool negval) ^ ")"
    | LD_COIL {input = _; variable = varname; negated = negval} -> "LD_COIL (variable : " ^ varname ^ ", neg : " ^ (string_of_bool negval) ^ ")"