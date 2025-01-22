open Base
open Stdio

let input_lines filename =
  let file = In_channel.create filename in
  In_channel.input_lines file

let instructions =
  List.nth_exn (input_lines "input.txt") 0
  |> String.split ~on:',' |> List.map ~f:Int.of_string

let replace list idx el =
  List.mapi list ~f:(fun i x -> if i = idx then el else x)

let execute instructions =
  let rec execute_impl instructions ptr inputs outputs =
    let parameters, opcode =
      List.nth_exn instructions ptr |> fun instruction ->
      (instruction / 100, instruction % 100)
    in
    match opcode with
    | 1 | 2 ->
        let parameter1 = parameters % 10 in
        let input1 =
          let arg = List.nth_exn instructions (ptr + 1) in
          if parameter1 = 0 then List.nth_exn instructions arg else arg
        in
        let parameter2 = parameters / 10 % 10 in
        let input2 =
          let arg = List.nth_exn instructions (ptr + 2) in
          if parameter2 = 0 then List.nth_exn instructions arg else arg
        in
        let write_pos = List.nth_exn instructions (ptr + 3) in
        let output = if opcode = 1 then input1 + input2 else input1 * input2 in
        let new_instructions = replace instructions write_pos output in
        execute_impl new_instructions (ptr + 4) inputs outputs
    | 3 ->
        assert (parameters = 0);
        let write_pos = List.nth_exn instructions (ptr + 1) in
        let input = List.hd_exn inputs in
        let new_instructions = replace instructions write_pos input in
        execute_impl new_instructions (ptr + 2) (List.tl_exn inputs) outputs
    | 4 ->
        let arg = List.nth_exn instructions (ptr + 1) in
        let new_output =
          if parameters = 0 then List.nth_exn instructions arg else arg
        in
        execute_impl instructions (ptr + 2) inputs (new_output :: outputs)
    | 99 -> outputs
    | _ -> raise (Failure (Printf.sprintf "Invalid opcode %d" opcode))
  in
  execute_impl instructions 0 [ 1 ] [] |> List.rev

let () =
  execute instructions |> List.map ~f:Int.to_string
  |> List.iter ~f:print_endline
