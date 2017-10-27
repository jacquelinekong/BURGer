type expr = Id of string
  | Call of string * expr list
  | Literal of int
  | String of string 

let rec string_of_expr = function
    Literal(l) -> l
  | Id(s) -> s
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
