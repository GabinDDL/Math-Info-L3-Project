let line_to_array l =
  let str_list = String.split_on_char ' ' l in
  let int_list = List.map (fun x -> int_of_string x) str_list in
  Array.of_list int_list

let file_to_array f =
  let ic = open_in f in
  let rec obtain_lines acc =
    try
      let l = line_to_array (input_line ic) in
      obtain_lines (l :: acc)
    with End_of_file -> List.rev acc
  in
  let lst_lines = obtain_lines [] in
  close_in ic;
  Array.of_list lst_lines
