open Base
open Stdio

let () =
  In_channel.create "input.txt"
  |> In_channel.input_lines |> List.iter ~f:print_endline
