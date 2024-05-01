let copy_matrix matrix =
  Array.init (Array.length matrix) (fun i -> Array.copy matrix.(i))

let check_neighbors_does_not_have_same_color grid height width current_row
    current_col color =
  let neighbors =
    [
      (current_row - 1, current_col);
      (current_row + 1, current_col);
      (current_row, current_col - 1);
      (current_row, current_col + 1);
    ]
  in
  List.for_all
    (fun (row, col) ->
      if row >= 0 && col >= 0 && row < height && col < width then
        grid.(row).(col) <> color
      else true)
    neighbors

let rec generate_colorings grid width height current_row current_col max_colors
    =
  if current_row = height then [ copy_matrix grid ]
  else if current_col = width then
    generate_colorings grid width height (current_row + 1) 0 max_colors
  else
    let rec for_all_color current_color acc =
      if current_color > max_colors then acc
      else if
        check_neighbors_does_not_have_same_color grid height width current_row
          current_col current_color
      then
        let () = grid.(current_row).(current_col) <- current_color in
        let copy =
          generate_colorings grid width height current_row (current_col + 1)
            max_colors
        in
        let () = grid.(current_row).(current_col) <- 0 in
        for_all_color (current_color + 1) (acc @ copy)
      else for_all_color (current_color + 1) acc
    in
    for_all_color 1 []

let generate_all_colorings width height max_colors =
  let grid = Array.make_matrix width height 0 in
  generate_colorings grid width height 0 0 max_colors

let check_graphs_differ_by_one_value_on_middle arr1 arr2 =
  let length_arr1 = Array.length arr1 in
  let length_arr2 = Array.length arr2 in
  if length_arr1 <> length_arr2 then false
  else
    let rec count_differences i j cmp =
      if cmp >= 2 then false
      else if j >= length_arr1 then cmp = 1
      else
        let length_sub_arr1 = Array.length arr1.(j) in
        let length_sub_arr2 = Array.length arr2.(j) in
        if length_sub_arr1 <> length_sub_arr2 then false
        else if i >= length_sub_arr1 then count_differences 0 (j + 1) cmp
        else if arr1.(j).(i) <> arr2.(j).(i) then
          if i = 1 && j = 1 then count_differences (i + 1) j (cmp + 1)
          else false
        else count_differences (i + 1) j cmp
    in
    count_differences 0 0 0

let count_number_tile_a_and_b set_tiles set_tilesA set_tilesB =
  List.fold_left
    (fun acc e ->
      if List.exists (fun x -> x = e) set_tilesA then acc + 1
      else if List.exists (fun x -> x = e) set_tilesB then acc + 1
      else acc)
    0 set_tiles

let is_parity_between_tiles_A_and_B graph set_tilesA set_tilesB =
  let tiles_from_graph =
    match graph with
    | [| [| a; b; c |]; [| d; e; f |]; [| g; h; i |] |] ->
        [
          [ [ a; b ]; [ d; e ] ];
          [ [ b; c ]; [ e; f ] ];
          [ [ d; e ]; [ g; h ] ];
          [ [ e; f ]; [ h; i ] ];
        ]
    | _ -> []
  in
  if tiles_from_graph = [] then false
  else
    let number_tile_a_and_b =
      count_number_tile_a_and_b tiles_from_graph set_tilesA set_tilesB
    in
    number_tile_a_and_b mod 2 = 0

let check_conserv_parity graphs nbr_graphs set_tilesA set_tilesB =
  let rec for_all_pairs i j count_pair conserv_Parity_list =
    if i >= nbr_graphs then (count_pair, conserv_Parity_list)
    else if j >= nbr_graphs then
      for_all_pairs (i + 1) (i + 2) count_pair conserv_Parity_list
    else if check_graphs_differ_by_one_value_on_middle graphs.(i) graphs.(j)
    then
      let parity_of_first_tile =
        is_parity_between_tiles_A_and_B graphs.(i) set_tilesA set_tilesB
      in
      let parity_of_second_tile =
        is_parity_between_tiles_A_and_B graphs.(j) set_tilesA set_tilesB
      in
      (* parity_of_first_tile <=> parity_of_second_tile *)
      let prop_check =
        ((not parity_of_first_tile) || parity_of_second_tile)
        && ((not parity_of_second_tile) || parity_of_first_tile)
      in
      for_all_pairs i (j + 1) (count_pair + 1)
        (prop_check :: conserv_Parity_list)
    else for_all_pairs i (j + 1) count_pair conserv_Parity_list
  in
  for_all_pairs 0 0 0 []

let start () =
  let width = 3 in
  let height = 3 in
  let max_colors = 4 in
  let set_tilesA = [ [ [ 2; 3 ]; [ 3; 1 ] ]; [ [ 1; 3 ]; [ 3; 2 ] ] ] in
  let set_tilesB =
    [
      [ [ 2; 1 ]; [ 1; 4 ] ];
      [ [ 3; 1 ]; [ 1; 4 ] ];
      [ [ 2; 1 ]; [ 3; 4 ] ];
      [ [ 2; 3 ]; [ 1; 4 ] ];
      [ [ 1; 3 ]; [ 4; 2 ] ];
      [ [ 3; 2 ]; [ 1; 4 ] ];
      [ [ 2; 3 ]; [ 3; 4 ] ];
      [ [ 4; 1 ]; [ 1; 2 ] ];
      [ [ 4; 1 ]; [ 1; 3 ] ];
      [ [ 4; 1 ]; [ 3; 2 ] ];
      [ [ 4; 3 ]; [ 1; 2 ] ];
      [ [ 2; 3 ]; [ 4; 1 ] ];
      [ [ 4; 2 ]; [ 1; 3 ] ];
      [ [ 4; 3 ]; [ 3; 2 ] ];
    ]
  in
  let tiles = Array.of_list (generate_all_colorings width height max_colors) in
  let count_graph = Array.length tiles in
  print_string "Number of graph with good coloring: ";
  print_int count_graph;
  print_newline ();
  let count_pair, result =
    check_conserv_parity tiles count_graph set_tilesA set_tilesB
  in
  print_string "Number of pairs obtainable with only changing the middle node: ";
  print_int count_pair;
  print_newline ();
  print_string "Parity is the same: ";
  if List.for_all (fun b -> b) result then print_string "true"
  else print_string "false";
  print_newline ()
