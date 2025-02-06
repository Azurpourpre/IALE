val list_fold_cat : ('a -> 'b list) -> 'a list -> 'b list
val follow : Xml.xml -> string list -> Xml.xml list
val parse_attr : (string * string) list -> string -> string option
val get_program : Xml.xml -> string -> Xml.xml