open Base
open Stdio

let input_lines = In_channel.create "input.txt" |> In_channel.input_lines
let () = List.iter ~f:print_endline input_lines
