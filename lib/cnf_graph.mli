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
val pp_sat : Format.formatter -> Sat.res -> int * int * int * int -> int -> unit

val get_name_variable : int * int * int * int -> int -> int
(** Bijective function that converts a quadruplet into an integer. *)

val get_variable_value : int -> int -> bool * int * int * int * int
(** Bijective function that converts an integer into corresponding quadruplet. *)

val pp_cnf : int -> Format.formatter -> cnf -> unit
val pp_clause : int -> Format.formatter -> clause -> unit

val get_only_one_true_cnf : int list -> cnf
(** Function to create a CNF that verifies that only one condition is true and the others are all false. *)

val check_has_color : int * int * int * int -> color list -> int -> cnf
(** Function that generates the cnf to check if a node is colored with a specific color and not with the others. *)

val check_has_not_color : int * int * int * int -> color list -> int -> cnf
(** Function that generates the cnf to check if a node is not colored with a specific color and is colored with a single other. *)

val check_coloration_of_one_node :
  int -> int -> int -> color list -> int -> int -> cnf
(** Function that generates the cnf to check if a node is correctly colored. *)

val check_coloration_of_graph : int -> int -> int -> int -> cnf
(** Function that generates the cnf that says if a graph is well colored at an instant t*)

val check_coloration_modification_of_graph :
  int -> int -> color list -> int -> cnf
(** Function that generates the cnf that says if a graph respects the rules of recoloring between t and t+1*)
