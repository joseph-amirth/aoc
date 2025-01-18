let read_file file_name =
  let file_handle = open_in file_name in
  let rec read_file_impl () =
    let maybe_line =
      try Some (input_line file_handle) with End_of_file -> None
    in
    match maybe_line with Some line -> line :: read_file_impl () | None -> []
  in
  read_file_impl ()

let input_lines = read_file "input.txt"
let () = List.iter print_endline input_lines
