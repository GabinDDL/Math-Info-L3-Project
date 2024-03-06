open SAT_Solver.Cnf
open Graph

let get_name_variable (x, y, t, c) dim =
  Printf.sprintf "%i" (x + (y * dim) + (t * dim * dim) + (c * dim * dim * dim))

let get_value_variable var dim =
  let x = int_of_string var in
  let not = x < 0 in
  let x = if not then -x else x in
  let c = x / (dim * dim * dim) in
  let x = x mod (dim * dim * dim) in
  let t = x / (dim * dim) in
  let x = x mod (dim * dim) in
  let y = x / dim in
  let x = x mod dim in
  (not, x, y, t, c)

let get_cnf_only_one_true (vars : string list) : cnf =
  let rec not_all_except var = function
    | [] -> []
    | hd :: tl -> [ (var, false); (hd, false) ] :: not_all_except var tl
  in
  let rec generate_exprs = function
    | [] -> []
    | hd :: tl -> not_all_except hd tl @ generate_exprs tl
  in
  List.map (fun x -> (x, true)) vars :: generate_exprs vars

let check_has_color (x, y, t, c) (possibles_colors : color list) (dim : int) :
    cnf =
  let rec aux res colors =
    match colors with
    | hd :: tl ->
        [
          (let var = get_name_variable (x, y, t, c) dim in
           if hd = c then (var, true) else (var, false));
        ]
        :: aux res tl
    | [] -> []
  in
  aux [] possibles_colors

let check_has_not_color (x, y, t, c) possibles_colors dim : cnf =
  [ (get_name_variable (x, y, t, c) dim, false) ]
  :: get_cnf_only_one_true
       (List.map
          (fun color -> get_name_variable (x, y, t, color) dim)
          possibles_colors)

let or_cnf_dev cnf1 cnf2 =
  List.fold_left
    (fun acc clause1 -> acc @ List.map (fun clause2 -> clause1 @ clause2) cnf2)
    [] cnf1

let check_coloration_of_one_node (x : int) (y : int) (t : int)
    (possibles_colors : color list) (dim : int) : cnf =
  let rec check_coord_have_one_color colors =
    match colors with
    | hd :: tl ->
        or_cnf_dev
          (check_has_not_color (x, y, t, hd) possibles_colors dim)
          (check_has_color ((x + 1) mod dim, y, t, hd) possibles_colors dim)
        @ or_cnf_dev
            (check_has_not_color (x, y, t, hd) possibles_colors dim)
            (check_has_color (x, (y + 1) mod dim, t, hd) possibles_colors dim)
        @ or_cnf_dev
            (check_has_not_color (x, y, t, hd) possibles_colors dim)
            (check_has_color
               ((x + dim - 1) mod dim, y, t, hd)
               possibles_colors dim)
        @ or_cnf_dev
            (check_has_not_color (x, y, t, hd) possibles_colors dim)
            (check_has_color
               (x, (y + dim - 1) mod dim, t, hd)
               possibles_colors dim)
        @ check_coord_have_one_color tl
    | [] -> []
  in
  check_coord_have_one_color possibles_colors
