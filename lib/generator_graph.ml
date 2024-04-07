let count = ref 0

let check_neighbors_does_not_have_same_color grid height width current_row
    current_col color =
  let neighbors =
    [
      ((current_row - 1 + height) mod height, current_col);
      ((current_row + 1) mod height, current_col);
      (current_row, (current_col - 1 + width) mod width);
      (current_row, (current_col + 1) mod width);
    ]
  in
  List.for_all (fun (row, col) -> grid.(row).(col) <> color) neighbors

let rec generate_colorings grid width height current_row current_col max_colors
    =
  if current_row = height then (
    Array.iter
      (fun current_row ->
        Array.iter
          (fun cell ->
            print_int cell;
            print_string " ")
          current_row;
        print_newline ())
      grid;
    count := !count + 1;
    print_newline ())
  else if current_col = width then
    generate_colorings grid width height (current_row + 1) 0 max_colors
  else
    let rec for_all_color current_color =
      if current_color > max_colors then ()
      else
        let () =
          if
            check_neighbors_does_not_have_same_color grid height width
              current_row current_col current_color
          then (
            grid.(current_row).(current_col) <- current_color;
            generate_colorings grid width height current_row (current_col + 1)
              max_colors;
            grid.(current_row).(current_col) <- 0)
        in
        for_all_color (current_color + 1)
    in
    for_all_color 1

let generate_all_colorings width height max_colors =
  let grid = Array.make_matrix width height 0 in
  generate_colorings grid width height 0 0 max_colors;
  print_int !count;
  print_newline ()
