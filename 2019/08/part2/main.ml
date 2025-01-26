open Base
open Stdio

let input_lines =
  let file = In_channel.create "input.txt" in
  In_channel.input_lines file

let width, height =
  let line = List.nth_exn input_lines 0 in
  let width_str, height_str = String.rsplit2 ~on:' ' line |> Option.value_exn in
  (Int.of_string width_str, Int.of_string height_str)

let digits =
  List.nth_exn input_lines 1 |> String.to_list |> List.map ~f:Char.get_digit_exn

let all_pixels =
  let length = width * height in
  List.chunks_of digits ~length |> List.transpose_exn

let visible_pixels =
  let get_pixel pixels =
    List.find_exn pixels ~f:(fun pixel -> pixel = 0 || pixel = 1)
  in
  List.map all_pixels ~f:get_pixel

let () =
  List.chunks_of visible_pixels ~length:width
  |> List.iter ~f:(fun row ->
         List.iter row ~f:(fun digit ->
             if digit = 1 then printf "#" else printf ".");
         printf "\n")
