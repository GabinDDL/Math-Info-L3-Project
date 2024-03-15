open Prjt_mi_recolor.Graph
open Prjt_mi_recolor.Parser

let run_solver_on_file file file_output =
  match parse_file_for_solver file with
  | Error e -> raise e
  | Ok ((t, w, l, _), a_init, a_final) -> (
      let grid_init = init_toroidal_grid 0 w l a_init in
      let grid_final = init_toroidal_grid t w l a_final in
      match file_output with
      | None -> Format.printf "%a\n\n%a" pp_graph grid_init pp_graph grid_final
      | Some output ->
          let ic = open_out output in
          Format.fprintf
            (Format.formatter_of_out_channel ic)
            "%a\n\n%a" pp_graph grid_init pp_graph grid_final;
          close_out ic)
(* TODO: To change with the result of SAT solver. *)

let () =
  let l = Array.length Sys.argv in
  if l = 2 then run_solver_on_file Sys.argv.(1) None
  else if l = 3 then run_solver_on_file Sys.argv.(1) (Some Sys.argv.(2))
  else failwith "Not a good number of arg"
