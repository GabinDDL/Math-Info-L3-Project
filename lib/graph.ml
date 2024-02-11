type color = Red | Blue | Green | Yellow
type position = int * int
type vertex = position * color
type neighbors = vertex list
type graph = int * (vertex * neighbors) list
type graph_traversal_error = UnknownVertex

let rec get_neighbors g v =
  match g with
  | _, [] -> Error UnknownVertex
  | t, (v_s, v_list) :: tl ->
      if v_s = v then Ok v_list else get_neighbors (t, tl) v

let is_vertex_in l v = List.mem v l
let is_vertex_of (_, s) v = List.exists (fun (v_s, _) -> v = v_s) s

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
