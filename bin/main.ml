open Prjt_mi_recolor.Cnf_graph
open Prjt_mi_recolor.Parser

let run_solver_on_file file file_output =
  match parse_file_for_solver file with
  | Error e -> raise e
  | Ok ((t, w, l, c), a_init, a_final) ->
      let solver = create_solver () in
      let res_cnf = get_cnf ((w, l), a_init) ((w, l), a_final) t c in
      add_clauses solver res_cnf;
      let fmt =
        match file_output with
        | None -> Format.std_formatter
        | Some output -> output |> open_out |> Format.formatter_of_out_channel
      in
      Format.fprintf fmt "%a\n%a" pp_res_solved (get_result solver)
        (pp_toroidal_grid_recoloration_solution (t, w, l, c)
           (Int.max (Int.max (Int.max w l) t) c + 2))
        (get_result solver)

(* TODO: To change with the result of SAT solver. *)

let () =
  let l = Array.length Sys.argv in
  if l = 2 then run_solver_on_file Sys.argv.(1) None
  else if l = 3 then run_solver_on_file Sys.argv.(1) (Some Sys.argv.(2))
  else failwith "Not a good number of arg"
