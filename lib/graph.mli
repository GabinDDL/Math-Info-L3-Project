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
