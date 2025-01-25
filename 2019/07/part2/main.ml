open Base
open Stdio

type state =
  | Start
  | Running of { instructions : int list; ptr : int; output : int }
  | Terminated

let execute instructions ?(ptr = 0) inputs =
  let rec execute_impl instructions ptr inputs =
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
        execute_impl new_instructions (ptr + 4) inputs
    | 3 ->
        assert (modes = 0);
        let write_pos = get (ptr + 1) in
        let input = List.hd_exn inputs in
        let new_instructions = set write_pos input in
        execute_impl new_instructions (ptr + 2) (List.tl_exn inputs)
    | 4 ->
        let arg = get (ptr + 1) in
        let output = if modes = 0 then get arg else arg in
        Running { instructions; ptr = ptr + 2; output }
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
        if condition then execute_impl instructions input2 inputs
        else execute_impl instructions (ptr + 3) inputs
    | 99 -> Terminated
    | _ -> raise (Failure (Printf.sprintf "Invalid opcode %d" opcode))
  in
  execute_impl instructions ptr inputs

let instructions =
  let file = In_channel.create "input.txt" in
  In_channel.input_lines file
  |> List.hd_exn |> String.split ~on:',' |> List.map ~f:Int.of_string

let () =
  let phase_combos =
    let phases = [ 5; 6; 7; 8; 9 ] in
    let rec gen amp combos =
      if amp = 5 then combos
      else
        let new_combos =
          List.map combos ~f:(fun combo ->
              let unused phase =
                List.find combo ~f:(Int.equal phase) |> Option.is_none
              in
              List.filter phases ~f:unused
              |> List.map ~f:(fun phase -> phase :: combo))
          |> List.concat
        in
        gen (amp + 1) new_combos
    in
    gen 0 [ [] ]
  in
  let solve phases =
    let rec solve_impl final_output states amp input =
      let new_state =
        match List.nth_exn states amp with
        | Start -> execute instructions [ List.nth_exn phases amp; input ]
        | Running { instructions; ptr; _ } ->
            execute instructions ~ptr [ input ]
        | Terminated -> failwith "Impossible"
      in
      match new_state with
      | Start -> failwith "Impossible"
      | Running { output; _ } ->
          let final_output = if amp = 4 then output else final_output in
          let new_states =
            List.mapi states ~f:(fun i state ->
                if i = amp then new_state else state)
          in
          solve_impl final_output new_states ((amp + 1) % 5) output
      | Terminated -> final_output
    in
    let init_states = List.init 5 ~f:(fun _ -> Start) in
    solve_impl 0 init_states 0 0
  in
  let answer =
    List.map phase_combos ~f:solve
    |> List.max_elt ~compare:Int.compare
    |> Option.value_exn
  in
  answer |> Int.to_string |> print_endline
