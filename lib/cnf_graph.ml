open Graph
module Sat = Msat_sat
module Element = Sat.Int_lit (* expressions *)

type literal = Element.t
type clause = literal list
type cnf = clause list

(** Create an element with a node value *)
let int_to_literal (value : int) : literal = Element.make value

let literal_to_int (e : literal) = Element.to_int e / 2

let pp_literal (fmt : Format.formatter) (e : literal) =
  Format.fprintf fmt "%d" (literal_to_int e)

let pp_cnf fmt (c : cnf) =
  List.iter
    (fun lst ->
      List.iter (fun e -> Format.fprintf fmt "%a " pp_literal e) lst;
      Format.fprintf fmt "\n")
    c

(** Create solver module *)
let create_solver () : Sat.solver = Sat.create ()

(** Get the negation of an element *)
let get_negation_of (elem : literal) : literal = Element.neg elem

(** Add list of clauses to the solver  *)
let add_clauses (solver : Sat.solver) (lst : literal list list) : unit =
  Sat.assume solver lst ()

(** Get the results of the CNF *)
let get_result (solver : Sat.solver) : Sat.res = Sat.solve solver

let get_name_variable (x, y, t, c) coef =
  x + (y * coef) + (t * coef * coef) + (c * coef * coef * coef) + 1

let get_variable_value var coef =
  let x = var - 1 in
  let not = x < 0 in
  let x = if not then -x else x in
  let c = x / (coef * coef * coef) in
  let x = x mod (coef * coef * coef) in
  let t = x / (coef * coef) in
  let x = x mod (coef * coef) in
  let y = x / coef in
  let x = x mod coef in
  (not, x, y, t, c)

let pp_res_solved (fmt : Format.formatter) (res : Sat.res) =
  match res with
  | Sat.Unsat _ -> Format.fprintf fmt "unsat\n"
  | Sat.Sat _ -> Format.fprintf fmt "sat\n"

let pp_sat (fmt : Format.formatter) (res : Sat.res)
    ((t_m, w, l, c_m) : int * int * int * int) dim =
  match res with
  | Sat.Sat a ->
      let rec print_res t x y c =
        if t > t_m then ()
        else if c > c_m then print_res t x (y + 1) 0
        else if y > l then print_res t (x + 1) 0 c
        else if x > w then print_res (t + 1) 0 y c
        else (
          Format.fprintf fmt "%d %d %d %d : %b\n" x y t c
            (a.eval (int_to_literal (get_name_variable (x, y, t, c) dim)));
          print_res t x y (c + 1))
      in
      print_res 0 0 0 0
  | _ -> ()

let get_only_one_true_cnf (vars : int list) : cnf =
  let rec get_all_two_tuple_at_less_was_false (cnf : cnf) = function
    | [] -> cnf
    | hd1 :: tl1 -> (
        match tl1 with
        | [] -> []
        | hd2 :: tl2 ->
            get_all_two_tuple_at_less_was_false
              ([
                 hd1 |> int_to_literal |> get_negation_of;
                 hd2 |> int_to_literal |> get_negation_of;
               ]
              :: get_all_two_tuple_at_less_was_false cnf tl2)
              tl1)
  in
  let rec at_less_one_true = function
    | [] -> []
    | hd :: tl -> (hd |> int_to_literal) :: at_less_one_true tl
  in
  at_less_one_true vars :: get_all_two_tuple_at_less_was_false [] vars

let check_each_case_has_only_one_color max_time w l
    (possible_colors : color list) : cnf =
  let coef = Int.max w l in
  let rec get_all_cnf_each_case_has_only_one_color time width length : cnf =
    if time < 0 then []
    else if width < 0 then
      get_all_cnf_each_case_has_only_one_color time w (length - 1)
    else if length < 0 then
      get_all_cnf_each_case_has_only_one_color (time - 1) w l
    else
      let rec get_all_color colors =
        match colors with
        | hd :: tl ->
            get_name_variable (width, length, time, hd) coef :: get_all_color tl
        | [] -> []
      in
      get_only_one_true_cnf (get_all_color possible_colors)
      @ get_all_cnf_each_case_has_only_one_color time (width - 1) length
  in
  get_all_cnf_each_case_has_only_one_color (max_time - 1) (w - 1) (l - 1)

let check_coloration_of_each_neighbor_is_different_for_each_node_of_the_graph w
    l max_time possible_colors : cnf =
  let check_coloration_of_each_neighbor_is_different_for_one_node (x : int)
      (y : int) (t : int) (possible_colors : color list) w l : cnf =
    let coef = Int.max w l in
    let rec check_coord_have_one_color colors =
      match colors with
      | hd :: tl ->
          [
            get_name_variable (x, y, t, hd) coef
            |> int_to_literal |> get_negation_of;
            get_name_variable ((x + 1) mod w, y, t, hd) coef
            |> int_to_literal |> get_negation_of;
          ]
          :: [
               get_name_variable (x, y, t, hd) coef
               |> int_to_literal |> get_negation_of;
               get_name_variable (x, (y + 1) mod l, t, hd) coef
               |> int_to_literal |> get_negation_of;
             ]
          :: [
               get_name_variable (x, y, t, hd) coef
               |> int_to_literal |> get_negation_of;
               get_name_variable ((x + w - 1) mod w, y, t, hd) coef
               |> int_to_literal |> get_negation_of;
             ]
          :: [
               get_name_variable (x, y, t, hd) coef
               |> int_to_literal |> get_negation_of;
               get_name_variable (x, (y + l - 1) mod l, t, hd) coef
               |> int_to_literal |> get_negation_of;
             ]
          :: check_coord_have_one_color tl
      | [] -> []
    in
    check_coord_have_one_color possible_colors
  in
  let rec check_coloration_of_each_node_each_time width height time =
    if width >= w then
      check_coloration_of_each_node_each_time 0 (height + 1) time
    else if height >= l then
      check_coloration_of_each_node_each_time 0 0 (time + 1)
    else if time >= max_time then []
    else
      check_coloration_of_each_neighbor_is_different_for_one_node width height
        time possible_colors w l
      @ check_coloration_of_each_node_each_time (width + 1) height time
  in
  check_coloration_of_each_node_each_time 0 0 0

let check_coloration_after_modification_of_graph w l time possible_colors =
  let coef = Int.max w l in
  let rec get_cnf_check_coloration_after_modification_fro_each_node width height
      =
    if width < 0 then
      get_cnf_check_coloration_after_modification_fro_each_node (w - 1)
        (height - 1)
    else if height < 0 then []
    else
      List.fold_left
        (fun acc color ->
          [
            get_name_variable (width, height, time, color) coef
            |> int_to_literal |> get_negation_of;
            get_name_variable ((width + 1) mod w, height, time + 1, color) coef
            |> int_to_literal |> get_negation_of;
          ]
          :: [
               get_name_variable (width, height, time, color) coef
               |> int_to_literal |> get_negation_of;
               get_name_variable
                 ((width + w - 1) mod w, height, time + 1, color)
                 coef
               |> int_to_literal |> get_negation_of;
             ]
          :: [
               get_name_variable (width, height, time, color) coef
               |> int_to_literal |> get_negation_of;
               get_name_variable
                 (width, (height + 1) mod l, time + 1, color)
                 coef
               |> int_to_literal |> get_negation_of;
             ]
          :: [
               get_name_variable (width, height, time, color) coef
               |> int_to_literal |> get_negation_of;
               get_name_variable
                 (width, (height + l - 1) mod l, time + 1, color)
                 coef
               |> int_to_literal |> get_negation_of;
             ]
          :: acc)
        [] possible_colors
      @ get_cnf_check_coloration_after_modification_fro_each_node (width - 1)
          height
  in
  get_cnf_check_coloration_after_modification_fro_each_node (w - 1) (l - 1)

let check_coloration_start_and_final ((w1, l1), graph1) graph2 max_time : cnf =
  let graph1 = Array.to_list graph1 in
  let graph2 = Array.to_list graph2 in
  let coef = Int.max w1 l1 in
  let rec get_all_clause y t graph is_second_graph : cnf =
    match graph with
    | hd :: tl ->
        let rec get_clause_for_line x hd : cnf =
          match hd with
          | fst_color :: other_color ->
              [ get_name_variable (x, y, t, fst_color) coef |> int_to_literal ]
              :: get_clause_for_line (x + 1) other_color
          | [] ->
              assert (x = w1);
              get_all_clause (y + 1) t tl is_second_graph
        in
        get_clause_for_line 0 (Array.to_list hd)
    | [] when is_second_graph -> []
    | [] ->
        assert (y = l1);
        get_all_clause 0 max_time graph2 true
  in
  get_all_clause 0 0 graph1 false

let get_cnf g1 g2 max_time nbr_colors =
  let possible_colors = List.init nbr_colors (fun x -> x) in
  match (g1, g2) with
  | ((l1, w1), a1), ((l2, w2), a2) when l1 = l2 && w1 = w2 ->
      let rec check_coloration_after_modification_of_graph_for_all_time t =
        if t <= 0 then []
        else
          check_coloration_after_modification_of_graph w1 l1 t possible_colors
          @ check_coloration_after_modification_of_graph_for_all_time (t - 1)
      in
      check_each_case_has_only_one_color max_time w1 l1 possible_colors
      @ check_coloration_start_and_final ((w1, l1), a1) a2 max_time
      @ check_coloration_after_modification_of_graph_for_all_time max_time
      @ check_coloration_of_each_neighbor_is_different_for_each_node_of_the_graph
          w1 l1 max_time possible_colors
  | ((l1, w1), _), ((l2, w2), _) ->
      assert (l1 = l2 && w1 = w2);
      []
(*never return this list*)
