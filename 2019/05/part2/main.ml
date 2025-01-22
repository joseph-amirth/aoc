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
    let modes, opcode =
      List.nth_exn instructions ptr |> fun instruction ->
      (instruction / 100, instruction % 100)
    in
    match opcode with
    | 1 | 2 | 7 | 8 ->
        let mode1 = modes % 10 in
        let input1 =
          let arg = List.nth_exn instructions (ptr + 1) in
          if mode1 = 0 then List.nth_exn instructions arg else arg
        in
        let mode2 = modes / 10 % 10 in
        let input2 =
          let arg = List.nth_exn instructions (ptr + 2) in
          if mode2 = 0 then List.nth_exn instructions arg else arg
        in
        let write_pos = List.nth_exn instructions (ptr + 3) in
        let output =
          match opcode with
          | 1 -> input1 + input2
          | 2 -> input1 * input2
          | 7 -> if input1 < input2 then 1 else 0
          | 8 -> if input1 = input2 then 1 else 0
          | _ -> raise (Failure (Printf.sprintf "Invalid opcode %d" opcode))
        in
        let new_instructions = replace instructions write_pos output in
        execute_impl new_instructions (ptr + 4) inputs outputs
    | 3 ->
        assert (modes = 0);
        let write_pos = List.nth_exn instructions (ptr + 1) in
        let input = List.hd_exn inputs in
        let new_instructions = replace instructions write_pos input in
        execute_impl new_instructions (ptr + 2) (List.tl_exn inputs) outputs
    | 4 ->
        let arg = List.nth_exn instructions (ptr + 1) in
        let output = if modes = 0 then List.nth_exn instructions arg else arg in
        execute_impl instructions (ptr + 2) inputs (output :: outputs)
    | 5 | 6 ->
        let mode1 = modes % 10 in
        let input1 =
          let arg = List.nth_exn instructions (ptr + 1) in
          if mode1 = 0 then List.nth_exn instructions arg else arg
        in
        let condition =
          (opcode = 5 && not (input1 = 0)) || (opcode = 6 && input1 = 0)
        in
        let mode2 = modes / 10 % 10 in
        let input2 =
          let arg = List.nth_exn instructions (ptr + 2) in
          if mode2 = 0 then List.nth_exn instructions arg else arg
        in
        if condition then execute_impl instructions input2 inputs outputs
        else execute_impl instructions (ptr + 3) inputs outputs
    | 99 -> outputs
    | _ -> raise (Failure (Printf.sprintf "Invalid opcode %d" opcode))
  in
  execute_impl instructions 0 [ 5 ] [] |> List.rev

let () =
  execute instructions |> List.map ~f:Int.to_string
  |> List.iter ~f:print_endline
