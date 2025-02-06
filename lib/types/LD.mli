type component = 
  | LD_LEFT_POWERRAIL
  | LD_RIGHT_POWERRAIL of int list
  | LD_CONTACT of {input: int list; variable: string; negated: bool;}
  | LD_COIL of {input: int list; variable : string; negated : bool;}

val format : component -> string