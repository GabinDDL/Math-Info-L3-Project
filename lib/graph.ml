type color = Red | Blue | Green | Yellow
type position = int * int
type vertex = position * color
type neighbors = vertex list
type graph = int * (vertex * neighbors) list
type graph_traversal_error = UnknownVertex

let pp_color fmt c =
  let c_str =
    match c with
    | Red -> "Red"
    | Blue -> "Blue"
    | Green -> "Green"
    | Yellow -> "Yellow"
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

let pp_graph fmt (t, lst) =
  Format.fprintf fmt "Time %d : %a@;" t pp_all_vertex_with_adjs lst

let rec get_neighbors g v =
  match g with
  | _, [] -> Error UnknownVertex
  | t, (v_s, v_list) :: tl ->
      if v_s = v then Ok v_list else get_neighbors (t, tl) v

let is_vertex_in l v = List.mem v l
let is_vertex_of (_, s) v = List.exists (fun (v_s, _) -> v = v_s) s

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
  let rec aux_init_grid g x y =
    let pos = (x, y) in
    let c = a.(y).(x) in
    let v = (pos, c) in
    let v_and_neighbors = (v, get_neighbors_toroidal_grid w l x y a) in
    if x = 0 && y = 0 then (t, v_and_neighbors :: g)
    else if x = 0 then aux_init_grid (v_and_neighbors :: g) (l - 1) (y - 1)
    else aux_init_grid (v_and_neighbors :: g) (x - 1) y
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
