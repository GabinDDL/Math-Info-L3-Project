exception Wrong_number_of_args
exception Wrong_width_of_array
exception Wrong_separation
exception Wrong_color_number

let parse_graph_informations (ic : in_channel) :
    (int * int * int * int, exn) result =
  try
    let l = input_line ic in
    let dim_str_list = String.split_on_char ' ' l in
    match dim_str_list with
    | [ t; w; l; c ] ->
        Ok (int_of_string t, int_of_string w, int_of_string l, int_of_string c)
    | _ -> raise Wrong_number_of_args
  with e -> Error e

let parse_argument_line_to_array (l : string) : int array =
  let str_list = String.split_on_char ' ' l in
  let int_list = List.map (fun x -> int_of_string x) str_list in
  Array.of_list int_list

let parse_file_to_array (ic : in_channel) ((w, l, c) : int * int * int) :
    (int array array, exn) result =
  let rec obtain_lines acc i =
    if i = l then Ok (List.rev acc)
    else
      try
        let l = parse_argument_line_to_array (input_line ic) in
        if Array.length l <> w
        then raise Wrong_width_of_array
        else if (Array.exists (fun n -> n < 0 || n > c) l)
        then raise Wrong_color_number
        else obtain_lines (l :: acc) (i + 1)
      with e -> Error e
  in
  match obtain_lines [] 0 with
  | Error e -> Error e
  | Ok lst_lines -> Ok (Array.of_list lst_lines)

let parse_graph_separator (ic : in_channel) : (unit, exn) result =
  try
    let sep = input_line ic in
    if String.compare sep "#" <> 0 then raise Wrong_separation else Ok ()
  with e -> Error e

let parse_file_for_solver (file : string) :
    ((int * int * int * int) * int array array * int array array, exn) result =
  let ic = open_in file in

  match parse_graph_informations ic with
  | Error e -> Error e
  | Ok (t, w, l, c) -> (
      match
        ( parse_file_to_array ic (w, l, c),
          parse_graph_separator ic,
          parse_file_to_array ic (w, l, c) )
      with
      | Ok init_a, Ok (), Ok final_a ->
          close_in ic;
          Ok ((t, w, l, c), init_a, final_a)
      | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e)
