exception Wrong_number_of_args

let parse_argument_line_to_array (l : string) : int array =
  let str_list = String.split_on_char ' ' l in
  let int_list = List.map (fun x -> int_of_string x) str_list in
  Array.of_list int_list

let parse_graph_informations (ic : in_channel) : (int * int * int, exn) result =
  try
    let l = input_line ic in
    let dim_str_list = String.split_on_char ' ' l in
    match dim_str_list with
    | [ t; w; l ] -> Ok (int_of_string t, int_of_string w, int_of_string l)
    | _ -> raise Wrong_number_of_args
  with e -> Error e

let parse_file_to_array (ic : in_channel) : (int array array, exn) result =
  let rec obtain_lines acc =
    try
      let l = parse_argument_line_to_array (input_line ic) in
      obtain_lines (l :: acc)
    with
    | End_of_file -> Ok (List.rev acc)
    | e -> Error e
  in
  match obtain_lines [] with
  | Error e -> Error e
  | Ok lst_lines -> Ok (Array.of_list lst_lines)

let parse_file_for_solver (file : string) :
    ((int * int * int) * int array array, exn) result =
  let ic = open_in file in
  let res =
    match (parse_graph_informations ic, parse_file_to_array ic) with
    | Error e, _ | _, Error e -> Error e
    | Ok info, Ok a -> Ok (info, a)
  in
  close_in ic;
  res
