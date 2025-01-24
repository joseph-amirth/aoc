open Base
open Stdio

let input_lines filename =
  let file = In_channel.create filename in
  In_channel.input_lines file

let input_lines = input_lines "input.txt"

let () =
  let orbits =
    List.map ~f:(String.rsplit2_exn ~on:')') input_lines
    |> List.map ~f:(fun (x, y) -> (y, x))
  in
  let graph = Map.of_alist_exn (module String) orbits in
  let path_from_root obj =
    let rec path_to_root_impl obj path =
      if String.(obj = "COM") then obj :: path
      else path_to_root_impl (Map.find_exn graph obj) (obj :: path)
    in
    path_to_root_impl obj []
  in
  let my_path = path_from_root "YOU" in
  let santa_path = path_from_root "SAN" in
  let common_path =
    let rec common_path_impl path1 path2 path =
      match (path1, path2) with
      | [], _ | _, [] -> path
      | hd1 :: tl1, hd2 :: tl2 ->
          if String.(hd1 = hd2) then common_path_impl tl1 tl2 (hd1 :: path)
          else path
    in
    common_path_impl my_path santa_path []
  in
  List.length my_path + List.length santa_path
  - (2 * List.length common_path)
  - 2
  |> Int.to_string |> print_endline
