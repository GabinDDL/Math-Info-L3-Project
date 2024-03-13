open SAT_Solver.Cnf
open Graph

val get_name_variable : int * int * int * int -> int -> string
(** Function that converts a quadruplet into a string. *)

val get_value_variable : string -> int -> bool * int * int * int * int
(** Function that converts a string into corresponding quadruplet. *)

val get_cnf_only_one_true : string list -> cnf
(** Function to create a CNF that verifies that only one condition is true and the others are all false. *)

val check_has_color : int * int * int * int -> color list -> int -> cnf
(** Function to check if a node is colored with a specific color and not with the others. *)

val check_has_not_color : int * int * int * int -> color list -> int -> cnf
(** Function to check if a node is not colored with a specific color and is colored with a single other. *)

val check_coloration_of_one_node : int -> int -> int -> color list -> int -> cnf
(** Function to check if a node is correctly colored. *)

val check_coloration_of_graph : int -> int -> int -> cnf
