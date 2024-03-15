exception Wrong_number_of_args
exception Wrong_width_of_array

val parse_file_for_solver :
  string -> ((int * int * int) * int array array * int array array, exn) result
(** Takes a path to a file to parse it and return the information needed to create a graph *)
