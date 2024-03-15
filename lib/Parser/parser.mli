val parse_file_for_solver :
  string -> ((int * int * int) * int array array, exn) result
(** Takes a path to a file to parse it and return the information needed to create a graph *)
