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

let input_line = List.hd (read_file "input.txt")

let numbers =
  let number_strings = String.split_on_char ',' input_line in
  List.map int_of_string number_strings

let rec process_numbers numbers pos =
  let opcode = List.nth numbers pos in
  if opcode == 99 then List.nth numbers 0
  else
    let input1_pos = List.nth numbers (pos + 1) in
    let input1 = List.nth numbers input1_pos in
    let input2_pos = List.nth numbers (pos + 2) in
    let input2 = List.nth numbers input2_pos in
    let output =
      match opcode with
      | 1 -> input1 + input2
      | 2 -> input1 * input2
      | _ -> raise (Failure (Printf.sprintf "Found unknown opcode %d" opcode))
    in
    let output_pos = List.nth numbers (pos + 3) in
    let new_numbers =
      List.mapi (fun i x -> if i == output_pos then output else x) numbers
    in
    process_numbers new_numbers (pos + 4)

let modified_numbers =
  List.mapi (fun i x -> match i with 1 -> 12 | 2 -> 2 | _ -> x) numbers

let answer = process_numbers modified_numbers 0
let _ = print_endline (string_of_int answer)
