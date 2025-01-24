open Base
open Stdio

let input_lines filename =
  let file = In_channel.create filename in
  In_channel.input_lines file

let input_lines = input_lines "input.txt"

let orbits =
  List.map ~f:(String.rsplit2_exn ~on:')') input_lines
  |> List.map ~f:(fun (x, y) -> (y, x))

let graph = Map.of_alist_exn (module String) orbits

let rec indirect_orbits obj =
  if String.(obj = "COM") then 0
  else 1 + indirect_orbits (Map.find_exn graph obj)

let answer = List.sum (module Int) (Map.keys graph) ~f:indirect_orbits
let () = answer |> Int.to_string |> print_endline
