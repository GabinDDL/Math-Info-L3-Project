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

let pp_res_solved (fmt : Format.formatter) (res : Sat.res) =
  match res with
  | Sat.Unsat _ -> Format.fprintf fmt "unsat\n"
  | Sat.Sat _ ->
      Format.fprintf fmt "sat\n"

let get_name_variable (x, y, t, c) dim =
  x + (y * dim) + (t * dim * dim) + (c * dim * dim * dim)

let get_variable_value var dim =
  let x = var in
  let not = x < 0 in
  let x = if not then -x else x in
  let c = x / (dim * dim * dim) in
  let x = x mod (dim * dim * dim) in
  let t = x / (dim * dim) in
  let x = x mod (dim * dim) in
  let y = x / dim in
  let x = x mod dim in
  (not, x, y, t, c)

let develop_or_cnf (cnf1 : cnf) (cnf2 : cnf) =
  List.fold_left
    (fun acc clause1 -> acc @ List.map (fun clause2 -> clause1 @ clause2) cnf2)
    [] cnf1

let get_only_one_true_cnf (vars : int list) : cnf =
  let rec only_var_true (var : int) = function
    | [] -> []
    | hd :: tl ->
        [
          (if var = hd then var |> int_to_literal
           else hd |> int_to_literal |> get_negation_of);
        ]
        :: only_var_true var tl
  in
  let rec generate_exprs = function
    | [] -> []
    | hd :: tl when tl = [] -> only_var_true hd vars
    | hd :: tl -> develop_or_cnf (only_var_true hd vars) (generate_exprs tl)
  in
  generate_exprs vars

let check_has_color (x, y, t, c) (possible_colors : color list) (dim : int) :
    cnf =
  let rec aux res colors =
    match colors with
    | hd :: tl ->
        [
          (let var = get_name_variable (x, y, t, c) dim in
           if hd = c then var |> int_to_literal
           else var |> int_to_literal |> get_negation_of);
        ]
        :: aux res tl
    | [] -> []
  in
  aux [] possible_colors

let check_has_not_color (x, y, t, c) possible_colors dim : cnf =
  [ get_name_variable (x, y, t, c) dim |> int_to_literal |> get_negation_of ]
  :: get_only_one_true_cnf
       (List.map
          (fun color -> get_name_variable (x, y, t, color) dim)
          possible_colors)

let check_coloration_of_one_node (x : int) (y : int) (t : int)
    (possible_colors : color list) (dim : int) : cnf =
  let rec check_coord_have_one_color colors =
    match colors with
    | hd :: tl ->
        develop_or_cnf
          (check_has_not_color (x, y, t, hd) possible_colors dim)
          (check_has_not_color ((x + 1) mod dim, y, t, hd) possible_colors dim
          @ check_has_not_color (x, (y + 1) mod dim, t, hd) possible_colors dim
          @ check_has_not_color
              ((x + dim - 1) mod dim, y, t, hd)
              possible_colors dim
          @ check_has_not_color
              (x, (y + dim - 1) mod dim, t, hd)
              possible_colors dim)
        @ check_coord_have_one_color tl
    | [] -> []
  in
  check_coord_have_one_color possible_colors

let check_coloration_of_graph dim max_time nbr_colors : cnf =
  let colors = List.init nbr_colors (fun x -> x) in
  let rec aux width height time =
    if width >= dim then aux 0 (height + 1) time
    else if height >= dim then aux 0 0 (time + 1)
    else if time >= max_time then []
    else
      check_coloration_of_one_node width height time colors dim
      @ aux (width + 1) height time
  in
  aux 0 0 0

let check_coloration_modification_of_graph dim time colors_list =
  let rec aux height width =
    if width < 0 then aux (height - 1) dim
    else if height < 0 then []
    else
      List.fold_left
        (fun acc color ->
          develop_or_cnf
            (check_has_not_color (width, height, time, color) colors_list dim)
            (check_has_not_color
               ((width + 1) mod dim, height, time + 1, color)
               colors_list dim
            @ check_has_not_color
                ((width + dim - 1) mod dim, height, time + 1, color)
                colors_list dim
            @ check_has_not_color
                (width, (height + 1) mod dim, time + 1, color)
                colors_list dim
            @ check_has_not_color
                (width, (height + dim - 1) mod dim, time + 1, color)
                colors_list dim)
          @ acc)
        [] colors_list
      @ aux height (width - 1)
  in
  aux (dim - 1) (dim - 1)
