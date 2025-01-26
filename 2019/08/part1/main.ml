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

let layers =
  let length = width * height in
  List.chunks_of digits ~length

let freqs =
  let freq layer =
    let result = Array.init 10 ~f:(fun _ -> 0) in
    List.iter layer ~f:(fun digit -> result.(digit) <- result.(digit) + 1);
    Array.to_list result
  in
  List.map layers ~f:freq

let () =
  let best_freq =
    List.min_elt freqs ~compare:(fun freq1 freq2 ->
        let zeroes1 = List.nth_exn freq1 0 in
        let zeroes2 = List.nth_exn freq2 0 in
        zeroes1 - zeroes2)
    |> Option.value_exn
  in
  let ones = List.nth_exn best_freq 1 in
  let twos = List.nth_exn best_freq 2 in
  printf "%d\n" (ones * twos)
