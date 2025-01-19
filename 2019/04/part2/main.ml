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
  let rec is_nondecreasing list =
    match list with
    | [] | [ _ ] -> true
    | first :: (second :: _ as tl) ->
        Char.( <= ) first second && is_nondecreasing tl
  in
  let runs list =
    let rec runs_impl list runs run last =
      match list with
      | [] -> run :: runs
      | hd :: tl ->
          if Char.( = ) hd last then runs_impl tl runs (hd :: run) last
          else runs_impl tl (run :: runs) [ hd ] hd
    in
    match list with [] -> [] | hd :: tl -> runs_impl tl [] [ hd ] hd
  in
  let str = Int.to_string num in
  let list = String.to_list str in
  is_nondecreasing list
  && List.exists ~f:(fun list -> List.length list = 2) (runs list)

let compute_answer lo hi =
  let rec impl i acc =
    if i > hi then acc else impl (i + 1) (Bool.to_int (is_password i) + acc)
  in
  impl lo 0

let () = compute_answer lo hi |> Int.to_string |> print_endline
