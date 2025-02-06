type t = {
  name: string;
  address: string option;
  globalID: int option;
  vartype : Types.M.iectype;
  initial_value: string option;
};;

val read : Xml.xml -> t list