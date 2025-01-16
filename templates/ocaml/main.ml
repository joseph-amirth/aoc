let read_file file_name =
  let rec read_file_impl file_handle =
    let open Option in
    let maybe_line =
      try Some (input_line file_handle) with End_of_file -> None
    in
    match maybe_line with
    | Some line -> line :: read_file_impl file_handle
    | None -> []
  in
  read_file_impl (open_in file_name)

let input_lines = read_file "input.txt"
let _ = List.iter print_endline input_lines
