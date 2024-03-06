open SAT_Solver.Cnf

type color = int
type position = int * int

type vertex = position * color
(** A vertex is a position with its color. *)

type neighbors = vertex list

type graph = {
  time : int;
  nb_vertex : int;
  vertex_with_neighbors : (vertex * neighbors) list;
}
(** A graph is a time and a list of vertex with its neighbors. *)

type graph_traversal_error = UnknownVertex

val get_neighbors : graph -> vertex -> (neighbors, graph_traversal_error) result
val is_vertex_of : graph -> vertex -> bool
val init_toroidal_grid : int -> int -> int -> color array array -> graph

val depth_first_search :
  graph -> vertex -> (vertex -> unit) -> (unit, graph_traversal_error) result

val breadth_first_search :
  graph -> vertex -> (vertex -> unit) -> (unit, graph_traversal_error) result

val pp_vertex : Format.formatter -> vertex -> unit
val pp_graph : Format.formatter -> graph -> unit

val get_name_variable : int * int * int * int -> int -> string
(** Function that converts a quadruplet into a string. *)

val get_value_variable : string -> int -> bool * int * int * int * int
(** Function that converts a string into corresponding quadruplet. *)

val get_cnf_only_one_true : string list -> cnf
val check_have_color : int * int * int * int -> color list -> int -> cnf
val check_have_not_color : int * int * int * int -> color list -> int -> cnf
val check_coloration : int -> int -> int -> color list -> int -> cnf
