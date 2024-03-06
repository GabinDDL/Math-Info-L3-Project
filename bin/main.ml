open Prjt_mi_recolor.Graph
open Prjt_mi_recolor.Parser

let () =
  pp_graph Format.std_formatter (init_toroidal_grid 0 5 5 (file_to_array "te"))
