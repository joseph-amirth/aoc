open Base
open Stdio

let execute list inputs =
  let rec execute_impl map ptr relative_base inputs outputs =
    let find i =
      Map.find map i |> Option.value_or_thunk ~default:(fun () -> 0)
    in
    let param pos mode =
      match mode with
      | 0 -> find pos
      | 1 -> pos
      | 2 -> find pos + relative_base
      | _ -> failwith (Printf.sprintf "Invalid mode %d" mode)
    in
    let set key data = Map.set map ~key ~data in
    let modes, opcode =
      find ptr |> fun instruction -> (instruction / 100, instruction % 100)
    in
    match opcode with
    | 1 | 2 | 7 | 8 ->
        let mode1 = modes % 10 in
        let input1 = param (ptr + 1) mode1 |> find in
        let mode2 = modes / 10 % 10 in
        let input2 = param (ptr + 2) mode2 |> find in
        let mode3 = modes / 100 % 10 in
        let write_pos = param (ptr + 3) mode3 in
        let output =
          match opcode with
          | 1 -> input1 + input2
          | 2 -> input1 * input2
          | 7 -> if input1 < input2 then 1 else 0
          | 8 -> if input1 = input2 then 1 else 0
          | _ -> failwith (Printf.sprintf "Invalid opcode %d" opcode)
        in
        let new_map = set write_pos output in
        execute_impl new_map (ptr + 4) relative_base inputs outputs
    | 3 ->
        let write_pos = param (ptr + 1) modes in
        let input = List.hd_exn inputs in
        let new_map = set write_pos input in
        execute_impl new_map (ptr + 2) relative_base (List.tl_exn inputs)
          outputs
    | 4 ->
        let output = param (ptr + 1) modes |> find in
        execute_impl map (ptr + 2) relative_base inputs (output :: outputs)
    | 5 | 6 ->
        let mode1 = modes % 10 in
        let input1 = param (ptr + 1) mode1 |> find in
        let condition =
          (opcode = 5 && not (input1 = 0)) || (opcode = 6 && input1 = 0)
        in
        let mode2 = modes / 10 % 10 in
        let input2 = param (ptr + 2) mode2 |> find in
        if condition then execute_impl map input2 relative_base inputs outputs
        else execute_impl map (ptr + 3) relative_base inputs outputs
    | 9 ->
        let input = param (ptr + 1) modes |> find in
        execute_impl map (ptr + 2) (relative_base + input) inputs outputs
    | 99 -> outputs
    | _ -> failwith (Printf.sprintf "Invalid opcode %d" opcode)
  in
  let map =
    List.mapi list ~f:(fun i x -> (i, x)) |> Map.of_alist_exn (module Int)
  in
  execute_impl map 0 0 inputs [] |> List.rev

let instructions =
  In_channel.create "input.txt" |> In_channel.input_lines |> fun input_lines ->
  List.nth_exn input_lines 0 |> String.split ~on:','
  |> List.map ~f:Int.of_string

let () =
  execute instructions [ 2 ] |> List.iter ~f:(printf "%d ");
  print_endline ""
