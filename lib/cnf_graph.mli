open Graph

module Sat = Msat_sat
module Element = Sat.Int_lit (* expressions *)

type literal = Element.t
type clause = literal list
type cnf = clause list

(** Create solver module *)
val create_solver : unit -> Sat.solver

(** Create an literal with a node value *)
val int_to_literal : int -> literal

(** Get the negation of a literal *)
val get_negation_of : literal -> literal

(** Add list of clauses to the solver  *)
val add_clauses : Sat.solver -> literal list list -> unit 

(** Get the results of the CNF *)
val get_result : Sat.solver -> Sat.res

val pp_res_solved : Format.formatter -> Sat.res -> unit

val get_name_variable : int * int * int * int -> int -> int
(** Bijective function that converts a quadruplet into an integer. *)

val get_variable_value : int -> int -> bool * int * int * int * int
(** Bijective function that converts an integer into corresponding quadruplet. *)

val get_only_one_true_cnf : int list -> cnf
(** Function to create a CNF that verifies that only one condition is true and the others are all false. *)

val check_has_color : int * int * int * int -> color list -> int -> cnf
(** Function that generates the cnf to check if a node is colored with a specific color and not with the others. *)

val check_has_not_color : int * int * int * int -> color list -> int -> cnf
(** Function that generates the cnf to check if a node is not colored with a specific color and is colored with a single other. *)

val check_coloration_of_one_node : int -> int -> int -> color list -> int -> cnf
(** Function that generates the cnf to check if a node is correctly colored. *)

val check_coloration_of_graph : int -> int -> int -> cnf
(** Function that generates the cnf that says if a graph is well colored at an instant t*)

val check_coloration_modification_of_graph : int -> int -> color list -> cnf
(** Function that generates the cnf that says if a graph respects the rules of recoloring between t and t+1*)

val pp_cnf : Format.formatter -> cnf -> unit
