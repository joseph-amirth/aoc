open Base
open Stdio

let instructions =
  let file = In_channel.create "input.txt" in
  In_channel.input_lines file
  |> List.hd_exn |> String.split ~on:',' |> List.map ~f:Int.of_string

let () = List.length instructions |> printf "Length: %d\n"

let execute instructions inputs =
  let rec execute_impl instructions ptr inputs outputs =
    let get i = List.nth_exn instructions i in
    let set i x =
      List.mapi instructions ~f:(fun j y -> if i = j then x else y)
    in
    let modes, opcode =
      get ptr |> fun instruction -> (instruction / 100, instruction % 100)
    in
    match opcode with
    | 1 | 2 | 7 | 8 ->
        let mode1 = modes % 10 in
        let input1 =
          let arg = get (ptr + 1) in
          if mode1 = 0 then get arg else arg
        in
        let mode2 = modes / 10 % 10 in
        let input2 =
          let arg = get (ptr + 2) in
          if mode2 = 0 then get arg else arg
        in
        let write_pos = get (ptr + 3) in
        let output =
          match opcode with
          | 1 -> input1 + input2
          | 2 -> input1 * input2
          | 7 -> if input1 < input2 then 1 else 0
          | 8 -> if input1 = input2 then 1 else 0
          | _ -> raise (Failure (Printf.sprintf "Invalid opcode %d" opcode))
        in
        let new_instructions = set write_pos output in
        execute_impl new_instructions (ptr + 4) inputs outputs
    | 3 ->
        assert (modes = 0);
        let write_pos = get (ptr + 1) in
        let input = List.hd_exn inputs in
        let new_instructions = set write_pos input in
        execute_impl new_instructions (ptr + 2) (List.tl_exn inputs) outputs
    | 4 ->
        let arg = get (ptr + 1) in
        let output = if modes = 0 then get arg else arg in
        execute_impl instructions (ptr + 2) inputs (output :: outputs)
    | 5 | 6 ->
        let mode1 = modes % 10 in
        let input1 =
          let arg = get (ptr + 1) in
          if mode1 = 0 then get arg else arg
        in
        let condition =
          (opcode = 5 && not (input1 = 0)) || (opcode = 6 && input1 = 0)
        in
        let mode2 = modes / 10 % 10 in
        let input2 =
          let arg = get (ptr + 2) in
          if mode2 = 0 then get arg else arg
        in
        if condition then execute_impl instructions input2 inputs outputs
        else execute_impl instructions (ptr + 3) inputs outputs
    | 99 -> outputs
    | _ -> raise (Failure (Printf.sprintf "Invalid opcode %d" opcode))
  in
  execute_impl instructions 0 inputs [] |> List.rev

let () =
  let phases = [ 0; 1; 2; 3; 4 ] in
  let rec exec_amp amp input used =
    let unused phase = List.find used ~f:(Int.equal phase) |> Option.is_none in
    let exec_helper phase =
      execute instructions [ phase; input ] |> List.hd_exn |> fun output ->
      if amp = 4 then output else exec_amp (amp + 1) output (phase :: used)
    in
    let outputs = List.filter phases ~f:unused |> List.map ~f:exec_helper in
    outputs |> List.max_elt ~compare:Int.compare |> Option.value_exn
  in
  exec_amp 0 0 [] |> Int.to_string |> print_endline
