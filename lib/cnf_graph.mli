open Graph
module Sat = Msat_sat
module Element = Sat.Int_lit (* expressions *)

type literal = Element.t
type clause = literal list
type cnf = clause list

val create_solver : unit -> Sat.solver
(** Create solver module *)

val int_to_literal : int -> literal
(** Create an literal with a node value *)

(** Get the negation of a literal *)
val get_negation_of : literal -> literal
(** Get the negation of an element *)

val add_clauses : Sat.solver -> literal list list -> unit
(** Add list of clauses to the solver  *)

val get_result : Sat.solver -> Sat.res
(** Get the results of the CNF *)

val pp_res_solved : Format.formatter -> Sat.res -> unit
val pp_sat : int * int * int * int -> int -> Format.formatter -> Sat.res -> unit

val pp_toroidal_grid_recoloration_solution :
  int * int * int * int -> int -> Format.formatter -> Sat.res -> unit

val get_name_variable : int * int * int * int -> int -> int
(** Bijective function that converts a quadruplet into an integer. *)

val get_variable_value : int -> int -> bool * int * int * int * int
(** Bijective function that converts an integer into corresponding quadruplet. *)

val pp_cnf : int -> Format.formatter -> cnf -> unit
val pp_clause : int -> Format.formatter -> clause -> unit

val get_only_one_true_cnf : int list -> cnf
(** Function that generates a CNF that verifies that only one condition is true and the others are all false. *)

val check_each_case_has_only_one_color :
  int -> int -> int -> color list -> int -> cnf
(** Function that generates a CNF that checks if each neighbor in a graph do not have the same color. *)

val check_coloration_of_each_neighbor_is_different_for_each_node_of_the_graph :
  int -> int -> int -> int list -> int -> cnf
(** Function that generates the cnf that says if a graph is well colored at an instant t. *)

val check_coloration_after_modification_of_graph :
  int -> int -> int -> color list -> int -> cnf
(** Function that generates the cnf that says if a graph respects the rules of recoloring between t and t+1. *)

val check_start_and_final_coloration :
  (int * int) * int array array -> int array array -> int -> int -> cnf
(** Function that generates a CNF that checks if a graph has the right start and end color.*)

val get_cnf :
  (int * int) * int array array ->
  (int * int) * int array array ->
  int ->
  int ->
  cnf
(** Function that generates a CNF indicating whether one can pass from graph 1 to graph 2 with successive recolorations.*)

val generate_random_4_coloration_graph : int -> int -> int array array option
val pp_graph : Format.formatter -> int array array -> unit
