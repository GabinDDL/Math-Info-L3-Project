type color = int
type position = int * int
type vertex = position * color
type neighbors = vertex list

type graph = {
  time : int;
  nb_vertex : int;
  vertex_with_neighbors : (vertex * neighbors) list;
}

type graph_traversal_error = UnknownVertex

let pp_color fmt c =
  let c_str =
    match c with
    | 0 -> "Red"
    | 1 -> "Blue"
    | 2 -> "Green"
    | 3 -> "Yellow"
    | 4 -> "White"
    | 5 -> "Black"
    | 6 -> "Purple"
    | 7 -> "Pink"
    | 8 -> "Orange"
    | 9 -> "Magenta"
    | _ -> "Other"
  in
  Format.fprintf fmt "%s" c_str

let pp_vertex fmt ((x, y), c) = Format.fprintf fmt "(%d, %d, %a)" x y pp_color c

let pp_adjs fmt adjs =
  adjs
  |> List.iter (fun v ->
         Format.fprintf fmt "%a" pp_vertex v;
         Format.fprintf fmt ";@;")

let pp_vertex_with_adjs fmt (v, adjs) =
  Format.fprintf fmt "%a : @[<v>%a@]@;" pp_vertex v pp_adjs adjs

let pp_all_vertex_with_adjs fmt lst =
  lst
  |> List.iter (fun v_with_adj ->
         Format.fprintf fmt "%a" pp_vertex_with_adjs v_with_adj;
         Format.fprintf fmt ";@;")

let pp_graph fmt g =
  Format.fprintf fmt "Time %d : %a@;" g.time pp_all_vertex_with_adjs
    g.vertex_with_neighbors

let get_neighbors g v =
  let rec aux_get_neighbors lst =
    match lst with
    | [] -> Error UnknownVertex
    | (v_s, v_list) :: tl -> if v_s = v then Ok v_list else aux_get_neighbors tl
  in
  aux_get_neighbors g.vertex_with_neighbors

let is_vertex_in l v = List.mem v l

let is_vertex_of g v =
  List.exists (fun (v_s, _) -> v = v_s) g.vertex_with_neighbors

let get_neighbors_toroidal_grid w l x y a =
  let n1 = ((if x = l - 1 then 0 else x + 1), y) in
  let n2 = (x, if y = w - 1 then 0 else y + 1) in
  let n3 = ((if x = 0 then l - 1 else x - 1), y) in
  let n4 = (x, if y = 0 then w - 1 else y - 1) in
  [
    (n1, a.(snd n1).(fst n1));
    (n2, a.(snd n2).(fst n2));
    (n3, a.(snd n3).(fst n3));
    (n4, a.(snd n4).(fst n4));
  ]

let init_toroidal_grid t w l a =
  let rec aux_init_grid lst x y =
    let pos = (x, y) in
    let c = a.(y).(x) in
    let v = (pos, c) in
    let v_and_neighbors = (v, get_neighbors_toroidal_grid w l x y a) in
    if x = 0 && y = 0 then
      {
        time = t;
        nb_vertex = w * l;
        vertex_with_neighbors = v_and_neighbors :: lst;
      }
    else if x = 0 then aux_init_grid (v_and_neighbors :: lst) (l - 1) (y - 1)
    else aux_init_grid (v_and_neighbors :: lst) (x - 1) y
  in
  aux_init_grid [] (l - 1) (w - 1)

let rec search g already_visited to_visit add app =
  match to_visit with
  | [] -> Ok ()
  | v :: tl -> (
      if is_vertex_in already_visited v then search g already_visited tl add app
      else
        let _ = app v in
        let already_visited = v :: already_visited in
        match get_neighbors g v with
        | Ok n ->
            let to_visit = add to_visit n in
            search g already_visited to_visit add app
        | Error e -> Error e)

let add_depth_fist_search s lst = lst @ s
let depth_first_search g v app = search g [] [ v ] add_depth_fist_search app
let add_breadth_first_search q lst = q @ lst

let breadth_first_search g v app =
  search g [] [ v ] add_breadth_first_search app

let get_name_variable (not, x, y, t, c) dim=
  Printf.sprintf "%s%i" (if not then "-" else "") (x + y * dim  + t * dim * dim + c * dim * dim * dim)

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