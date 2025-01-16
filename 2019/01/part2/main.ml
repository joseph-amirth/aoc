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
let module_masses = List.map int_of_string input_lines

let module_fuel_reqs =
  let rec calc_fuel_req mass =
    let fuel_req = (mass / 3) - 2 in
    if fuel_req > 0 then fuel_req + calc_fuel_req fuel_req else 0
  in
  List.map calc_fuel_req module_masses

let answer = List.fold_left ( + ) 0 module_fuel_reqs
let () = print_endline (Int.to_string answer)
