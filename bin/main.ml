open Prjt_mi_recolor.Graph
open Prjt_mi_recolor.Parser

let run_solver_on_file file =
    Format.printf "%s" file;
  match parse_file_for_solver file with
  | Error e -> raise e
  | Ok ((l, w), a) ->
      let grid = init_toroidal_grid 0 l w a in
      pp_graph Format.std_formatter grid

let () =
  let l = Array.length Sys.argv in
  if l > 3 || l <= 1 then failwith "Not a good number of arg"
  else run_solver_on_file Sys.argv.(1)
