open Graph
module Sat = Msat_sat
module Element = Sat.Int_lit (* expressions *)

type literal = Element.t
type clause = literal list
type cnf = clause list

(** Create an element with a node value *)
let int_to_literal (value : int) : literal = Element.make value

let literal_to_int (e : literal) = Element.to_int e / 2

(** Create solver module *)
let create_solver () : Sat.solver = Sat.create ()

(** Get the negation of an element *)
let get_negation_of (elem : literal) : literal = Element.neg elem

(** Add list of clauses to the solver  *)
let add_clauses (solver : Sat.solver) (lst : literal list list) : unit =
  Sat.assume solver lst ()

(** Get the results of the CNF *)
let get_result (solver : Sat.solver) : Sat.res = Sat.solve solver

let get_name_variable (x, y, t, c) max_param =
  x + (y * max_param)
  + (t * max_param * max_param)
  + (c * max_param * max_param * max_param)
  + 1

let get_variable_value var max_param =
  let x = var - 1 in
  let not = x < 0 in
  let x = if not then -x else x in
  let c = x / (max_param * max_param * max_param) in
  let x = x mod (max_param * max_param * max_param) in
  let t = x / (max_param * max_param) in
  let x = x mod (max_param * max_param) in
  let y = x / max_param in
  let x = x mod max_param in
  (not, x, y, t, c)

let pp_var max_param fmt var =
  let not, x, y, t, c = get_variable_value var max_param in
  if not then Format.fprintf fmt "not (%d, %d, %d, %d)" x y t c
  else Format.fprintf fmt "(%d, %d, %d, %d)" x y t c

let pp_literal max_param (fmt : Format.formatter) (e : literal) =
  Format.fprintf fmt "%a" (pp_var max_param) (literal_to_int e)

let pp_clause max_param fmt (c : clause) =
  List.iter (fun e -> Format.fprintf fmt "%a; " (pp_literal max_param) e) c

let pp_cnf max_param fmt (c : cnf) =
  List.iter (fun cl -> Format.fprintf fmt "%a\n" (pp_clause max_param) cl) c

let pp_res_solved (fmt : Format.formatter) (res : Sat.res) =
  match res with
  | Sat.Unsat _ -> Format.fprintf fmt "unsat\n"
  | Sat.Sat _ -> Format.fprintf fmt "sat\n"

let pp_sat (fmt : Format.formatter) (res : Sat.res)
    ((t_m, w, l, c_m) : int * int * int * int) max_param =
  match res with
  | Sat.Sat a ->
      let rec print_res t x y c =
        if t > t_m then ()
        else if c > c_m then print_res t x (y + 1) 0
        else if y > l then print_res t (x + 1) 0 c
        else if x > w then print_res (t + 1) 0 y c
        else (
          Format.fprintf fmt "%d %d %d %d : %b\n" x y t c
            (a.eval (int_to_literal (get_name_variable (x, y, t, c) max_param)));
          print_res t x y (c + 1))
      in
      print_res 0 0 0 0
  | _ -> ()

let get_only_one_true_cnf (vars : int list) : cnf =
  let rec get_all_two_tuple_at_least_was_false = function
    | [] -> []
    | hd :: tl ->
        List.map
          (fun e ->
            [
              e |> int_to_literal |> get_negation_of;
              hd |> int_to_literal |> get_negation_of;
            ])
          tl
        @ get_all_two_tuple_at_least_was_false tl
  in
  let rec at_least_one_true = function
    | [] -> []
    | hd :: tl -> (hd |> int_to_literal) :: at_least_one_true tl
  in
  at_least_one_true vars :: get_all_two_tuple_at_least_was_false vars

let check_each_case_has_only_one_color max_time w l
    (possible_colors : color list) max_param : cnf =
  let rec get_all_cnf_each_case_has_only_one_color time width length : cnf =
    if time < 0 then []
    else if width < 0 then
      get_all_cnf_each_case_has_only_one_color time (w - 1) (length - 1)
    else if length < 0 then
      get_all_cnf_each_case_has_only_one_color (time - 1) (w - 1) (l - 1)
    else
      let rec get_all_color colors =
        match colors with
        | hd :: tl ->
            get_name_variable (width, length, time, hd) max_param
            :: get_all_color tl
        | [] -> []
      in
      get_only_one_true_cnf (get_all_color possible_colors)
      @ get_all_cnf_each_case_has_only_one_color time (width - 1) length
  in
  get_all_cnf_each_case_has_only_one_color max_time (w - 1) (l - 1)

let check_coloration_of_each_neighbor_is_different_for_each_node_of_the_graph w
    l max_time possible_colors max_param : cnf =
  let check_coloration_of_each_neighbor_is_different_for_one_node (x : int)
      (y : int) (t : int) (possible_colors : color list) : cnf =
    let rec check_coord_have_one_color colors =
      match colors with
      | hd :: tl ->
          [
            get_name_variable (x, y, t, hd) max_param
            |> int_to_literal |> get_negation_of;
            get_name_variable ((x + 1) mod w, y, t, hd) max_param
            |> int_to_literal |> get_negation_of;
          ]
          :: [
               get_name_variable (x, y, t, hd) max_param
               |> int_to_literal |> get_negation_of;
               get_name_variable (x, (y + 1) mod l, t, hd) max_param
               |> int_to_literal |> get_negation_of;
             ]
          :: [
               get_name_variable (x, y, t, hd) max_param
               |> int_to_literal |> get_negation_of;
               get_name_variable ((x + w - 1) mod w, y, t, hd) max_param
               |> int_to_literal |> get_negation_of;
             ]
          :: [
               get_name_variable (x, y, t, hd) max_param
               |> int_to_literal |> get_negation_of;
               get_name_variable (x, (y + l - 1) mod l, t, hd) max_param
               |> int_to_literal |> get_negation_of;
             ]
          :: check_coord_have_one_color tl
      | [] -> []
    in
    check_coord_have_one_color possible_colors
  in
  let rec check_coloration_of_each_node_each_time width height time =
    if width < 0 then
      check_coloration_of_each_node_each_time (w - 1) (height - 1) time
    else if height < 0 then
      check_coloration_of_each_node_each_time (w - 1) (l - 1) (time - 1)
    else if time < 0 then []
    else
      check_coloration_of_each_neighbor_is_different_for_one_node width height
        time possible_colors
      @ check_coloration_of_each_node_each_time (width - 1) height time
  in
  check_coloration_of_each_node_each_time (w - 1) (l - 1) max_time

let check_coloration_after_modification_of_graph w l t possible_colors max_param
    : cnf =
  let rec get_cnf_check_coloration_after_modification_for_each_node width height
      time =
    if width < 0 then
      get_cnf_check_coloration_after_modification_for_each_node (w - 1)
        (height - 1) time
    else if height < 0 then
      get_cnf_check_coloration_after_modification_for_each_node (w - 1) (l - 1)
        (time - 1)
    else if time < 0 then []
    else
      List.fold_left
        (fun acc color ->
          [
            get_name_variable (width, height, time, color) max_param
            |> int_to_literal |> get_negation_of;
            get_name_variable
              ((width + 1) mod w, height, time + 1, color)
              max_param
            |> int_to_literal |> get_negation_of;
          ]
          :: [
               get_name_variable (width, height, time, color) max_param
               |> int_to_literal |> get_negation_of;
               get_name_variable
                 ((width + w - 1) mod w, height, time + 1, color)
                 max_param
               |> int_to_literal |> get_negation_of;
             ]
          :: [
               get_name_variable (width, height, time, color) max_param
               |> int_to_literal |> get_negation_of;
               get_name_variable
                 (width, (height + 1) mod l, time + 1, color)
                 max_param
               |> int_to_literal |> get_negation_of;
             ]
          :: [
               get_name_variable (width, height, time, color) max_param
               |> int_to_literal |> get_negation_of;
               get_name_variable
                 (width, (height + l - 1) mod l, time + 1, color)
                 max_param
               |> int_to_literal |> get_negation_of;
             ]
          :: acc)
        [] possible_colors
      @ get_cnf_check_coloration_after_modification_for_each_node (width - 1)
          height time
  in
  get_cnf_check_coloration_after_modification_for_each_node (w - 1) (l - 1) t

let check_start_and_final_coloration ((w1, l1), graph1) graph2 max_time
    max_param : cnf =
  let graph1 = Array.to_list graph1 in
  let graph2 = Array.to_list graph2 in
  let rec get_all_clause y t graph is_second_graph : cnf =
    match graph with
    | hd :: tl ->
        let rec get_clause_for_line x hd : cnf =
          match hd with
          | fst_color :: other_color ->
              [
                get_name_variable (x, y, t, fst_color) max_param
                |> int_to_literal;
              ]
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
      let max_param =
        Int.max (Int.max (Int.max w1 l1) max_time) nbr_colors + 2
      in
      check_each_case_has_only_one_color max_time w1 l1 possible_colors
        max_param
      @ check_start_and_final_coloration ((w1, l1), a1) a2 max_time max_param
      @ check_coloration_after_modification_of_graph w1 l1 max_time
          possible_colors max_param
      @ check_coloration_of_each_neighbor_is_different_for_each_node_of_the_graph
          w1 l1 max_time possible_colors max_param
  | ((l1, w1), _), ((l2, w2), _) ->
      assert (l1 = l2 && w1 = w2);
      []
(*never return this list*)
