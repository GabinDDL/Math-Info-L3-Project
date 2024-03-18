open SAT_Solver.Cnf
open Graph

let get_name_variable (x, y, t, c) coef =
  Printf.sprintf "%i"
    (x + (y * coef) + (t * coef * coef) + (c * coef * coef * coef))

let get_variable_value var coef =
  let x = int_of_string var in
  let not = x < 0 in
  let x = if not then -x else x in
  let c = x / (coef * coef * coef) in
  let x = x mod (coef * coef * coef) in
  let t = x / (coef * coef) in
  let x = x mod (coef * coef) in
  let y = x / coef in
  let x = x mod coef in
  (not, x, y, t, c)

let develop_or_cnf (cnf1 : cnf) (cnf2 : cnf) =
  List.fold_left
    (fun acc clause1 -> acc @ List.map (fun clause2 -> clause1 @ clause2) cnf2)
    [] cnf1

let get_only_one_true_cnf (vars : string list) : cnf =
  let rec get_all_two_tuple_at_less_was_false (cnf : cnf) = function
    | [] -> cnf
    | hd1 :: tl1 -> (
        match tl1 with
        | [] -> []
        | hd2 :: tl2 ->
            get_all_two_tuple_at_less_was_false
              ([ (hd1, false); (hd2, false) ]
              :: get_all_two_tuple_at_less_was_false cnf tl2)
              tl1)
  in
  let rec at_less_one_true = function
    | [] -> []
    | hd :: tl -> (hd, true) :: at_less_one_true tl
  in
  at_less_one_true vars :: get_all_two_tuple_at_less_was_false [] vars

let check_has_color (x, y, t, c) (possible_colors : color list) w l : cnf =
  let coef = Int.max w l in
  let rec aux res colors =
    match colors with
    | hd :: tl ->
        [
          (let var = get_name_variable (x, y, t, hd) coef in
           if hd = c then (var, true) else (var, false));
        ]
        :: aux res tl
    | [] -> []
  in
  aux [] possible_colors

let check_has_not_color (x, y, t, c) possible_colors w l : cnf =
  let coef = Int.max w l in
  [ (get_name_variable (x, y, t, c) coef, false) ]
  :: get_only_one_true_cnf
       (List.map
          (fun color -> get_name_variable (x, y, t, color) coef)
          possible_colors)

let check_coloration_of_one_node (x : int) (y : int) (t : int)
    (possible_colors : color list) w l : cnf =
  let coef = Int.max w l in
  let rec check_coord_have_one_color colors =
    match colors with
    | hd :: tl ->
        [
          (get_name_variable (x, y, t, hd) coef, false);
          (get_name_variable ((x + 1) mod w, y, t, hd) coef, false);
        ]
        :: [
             (get_name_variable (x, y, t, hd) coef, false);
             (get_name_variable (x, (y + 1) mod l, t, hd) coef, false);
           ]
        :: [
             (get_name_variable (x, y, t, hd) coef, false);
             (get_name_variable ((x + w - 1) mod w, y, t, hd) coef, false);
           ]
        :: [
             (get_name_variable (x, y, t, hd) coef, false);
             (get_name_variable (x, (y + l - 1) mod l, t, hd) coef, false);
           ]
        :: check_coord_have_one_color tl
    | [] -> []
  in
  check_coord_have_one_color possible_colors

let check_coloration_of_graph w l max_time possible_colors : cnf =
  let rec aux width height time =
    if width >= w then aux 0 (height + 1) time
    else if height >= l then aux 0 0 (time + 1)
    else if time >= max_time then []
    else
      check_coloration_of_one_node width height time possible_colors w l
      @ aux (width + 1) height time
  in
  aux 0 0 0

let check_coloration_modification_of_graph w l time possible_colors =
  let coef = Int.max w l in
  let rec aux width height =
    if width < 0 then aux (w - 1) (height - 1)
    else if height < 0 then []
    else
      List.fold_left
        (fun acc color ->

          [
          (get_name_variable (width, height, time, color) coef, false);
          (get_name_variable ((width + 1) mod w, height, time + 1, color) coef, false);
        ] ::
        [
          (get_name_variable (width, height, time, color) coef, false);
          (get_name_variable ((width + w - 1) mod w, height, time + 1, color) coef, false);
        ] ::
        [
          (get_name_variable (width, height, time, color) coef, false);
          (get_name_variable (width, (height + 1) mod l, time + 1, color) coef, false);
        ] ::
        [
          (get_name_variable (width, height, time, color) coef, false);
          (get_name_variable (width, (height + l - 1) mod l, time + 1, color) coef, false);
        ] ::
          acc)
        [] possible_colors
      @ aux (width - 1) height
  in
  aux (w - 1) (l - 1)

let check_coloration_start_and_final ((l1, w1), a1) a2 max_time colors_list :
    cnf =
  let aux t i e =
    List.fold_left
      (fun acc e -> acc @ e)
      []
      (Array.to_list
         (Array.mapi
            (fun j c -> check_has_color (i, j, t, c) colors_list w1 l1)
            e))
  in
  List.fold_left
    (fun acc e -> acc @ e)
    []
    (Array.to_list (Array.mapi (aux 0) a1))
  @ List.fold_left
      (fun acc e -> acc @ e)
      []
      (Array.to_list (Array.mapi (aux max_time) a2))

let get_cnf g1 g2 max_time nbr_colors =
  let possible_colors = List.init nbr_colors (fun x -> x) in
  match (g1, g2) with
  | ((l1, w1), a1), ((l2, w2), a2) when l1 = l2 && w1 = w2 ->
      let rec check_coloration_modification_of_graph_for_all_time t =
        if t <= 0 then []
        else
          check_coloration_modification_of_graph w1 l1 t possible_colors
          @ check_coloration_modification_of_graph_for_all_time (t - 1)
      in
      check_coloration_start_and_final
        ((l1, w1), a1)
        a2 max_time possible_colors
      @ check_coloration_modification_of_graph_for_all_time max_time
      @ check_coloration_of_graph w1 l1 max_time possible_colors
  | _ -> assert false
(* check l1 = l2 && w1 = w2 *)
