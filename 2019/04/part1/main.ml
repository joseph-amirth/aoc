open Base
open Stdio

let input_lines filename =
  let file = In_channel.create filename in
  In_channel.input_lines file

let lo, hi =
  let input_lines = input_lines "input.txt" in
  let lo_str, hi_str =
    List.nth_exn input_lines 0 |> String.rsplit2_exn ~on:'-'
  in
  (Int.of_string lo_str, Int.of_string hi_str)

let is_password num =
  let str = Int.to_string num in
  let list = String.to_list str in
  let suf = List.drop list 1 in
  let pref = List.drop_last_exn list in
  let adjacent_same = List.exists2_exn ~f:Char.equal pref suf in
  let nondecreasing = List.for_all2_exn ~f:Char.( <= ) pref suf in
  nondecreasing && adjacent_same

let compute_answer lo hi =
  let rec impl i acc =
    if i > hi then acc else impl (i + 1) (Bool.to_int (is_password i) + acc)
  in
  impl lo 0

let () = compute_answer lo hi |> Int.to_string |> print_endline
