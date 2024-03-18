open SAT_Solver.Cnf
open Graph

val get_name_variable : int * int * int * int -> int -> string
(** Function that converts a quadruplet into a string. *)

val get_variable_value : string -> int -> bool * int * int * int * int
(** Function that converts a string into corresponding quadruplet. *)

val get_only_one_true_cnf : string list -> cnf
(** Function to create a CNF that verifies that only one condition is true and the others are all false. *)

val check_each_case_has_only_one_color : int -> int -> int -> color list -> cnf

val check_coloration_of_one_node :
  int -> int -> int -> color list -> int -> int -> cnf
(** Function that generates the cnf to check if a node is correctly colored. *)

val check_coloration_of_graph : int -> int -> int -> int list -> cnf
(** Function that generates the cnf that says if a graph is well colored at an instant t*)

val check_coloration_modification_of_graph :
  int -> int -> int -> color list -> cnf
(** Function that generates the cnf that says if a graph respects the rules of recoloring between t and t+1*)

val check_coloration_start_and_final :
  (int * int) * int array array -> int array array -> int -> color list -> cnf

val get_cnf :
  (int * int) * int array array ->
  (int * int) * int array array ->
  int ->
  int ->
  cnf
