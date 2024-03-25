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

let check_has_color (x, y, t, c) (possible_colors : color list)
    (max_param : int) : cnf =
  let rec aux res colors =
    match colors with
    | hd :: tl ->
        [
          (let var = get_name_variable (x, y, t, c) max_param in
           if hd = c then var |> int_to_literal
           else var |> int_to_literal |> get_negation_of);
        ]
        :: aux res tl
    | [] -> []
  in
  aux [] possible_colors

let check_has_not_color (x, y, t, c) possible_colors max_param : cnf =
  [
    get_name_variable (x, y, t, c) max_param
    |> int_to_literal |> get_negation_of;
  ]
  :: get_only_one_true_cnf
       (List.map
          (fun color -> get_name_variable (x, y, t, color) max_param)
          possible_colors)

let check_coloration_of_one_node (x : int) (y : int) (t : int)
    (possible_colors : color list) (dim : int) (max_param : int) : cnf =
  let rec check_coord_have_one_color colors =
    match colors with
    | hd :: tl ->
        develop_or_cnf
          (check_has_not_color (x, y, t, hd) possible_colors max_param)
          (check_has_not_color
             ((x + 1) mod dim, y, t, hd)
             possible_colors max_param
          @ check_has_not_color
              (x, (y + 1) mod dim, t, hd)
              possible_colors max_param
          @ check_has_not_color
              ((x + dim - 1) mod dim, y, t, hd)
              possible_colors max_param
          @ check_has_not_color
              (x, (y + dim - 1) mod dim, t, hd)
              possible_colors max_param)
        @ check_coord_have_one_color tl
    | [] -> []
  in
  check_coord_have_one_color possible_colors

let check_coloration_of_graph dim max_time nbr_colors max_param : cnf =
  let colors = List.init nbr_colors (fun x -> x) in
  let rec aux width height time =
    if width >= dim then aux 0 (height + 1) time
    else if height >= dim then aux 0 0 (time + 1)
    else if time >= max_time then []
    else
      check_coloration_of_one_node width height time colors dim max_param
      @ aux (width + 1) height time
  in
  aux 0 0 0

let check_coloration_modification_of_graph dim time colors_list max_param =
  let rec aux height width =
    if width < 0 then aux (height - 1) dim
    else if height < 0 then []
    else
      List.fold_left
        (fun acc color ->
          develop_or_cnf
            (check_has_not_color
               (width, height, time, color)
               colors_list max_param)
            (check_has_not_color
               ((width + 1) mod dim, height, time + 1, color)
               colors_list max_param
            @ check_has_not_color
                ((width + dim - 1) mod dim, height, time + 1, color)
                colors_list max_param
            @ check_has_not_color
                (width, (height + 1) mod dim, time + 1, color)
                colors_list max_param
            @ check_has_not_color
                (width, (height + dim - 1) mod dim, time + 1, color)
                colors_list max_param)
          @ acc)
        [] colors_list
      @ aux height (width - 1)
  in
  aux (dim - 1) (dim - 1)
