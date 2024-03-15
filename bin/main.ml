open Prjt_mi_recolor.Graph
open Prjt_mi_recolor.Parser

let run_solver_on_file file file_output =
  match parse_file_for_solver file with
  | Error e -> raise e
  | Ok ((t, l, w), a) -> (
      let grid = init_toroidal_grid t w l a in
      match file_output with
      | None -> pp_graph Format.std_formatter grid
      | Some output ->
          let ic = open_out output in
          pp_graph (Format.formatter_of_out_channel ic) grid;
          close_out ic)
(* TODO: To change with the result of SAT solver. *)

let () =
  let l = Array.length Sys.argv in
  if l = 2 then run_solver_on_file Sys.argv.(1) None
  else if l = 3 then run_solver_on_file Sys.argv.(1) (Some Sys.argv.(2))
  else failwith "Not a good number of arg"
