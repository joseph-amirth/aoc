open Base
open Stdio

let input_lines filename =
  let file = In_channel.create filename in
  In_channel.input_lines file

let input_lines = input_lines "input.txt"
let () = List.iter ~f:print_endline input_lines
