open Base
open Stdio

type instructions = (int, int, Base.Int.comparator_witness) Map.t

type result =
  | NeedInput of {
      instructions : instructions;
      ptr : int;
      relative_base : int;
      outputs : int list;
    }
  | Terminated of { outputs : int list }

let execute instructions ptr relative_base inputs =
  let rec execute_impl instructions ptr relative_base inputs outputs =
    let find i =
      Map.find instructions i |> Option.value_or_thunk ~default:(fun () -> 0)
    in
    let param pos mode =
      match mode with
      | 0 -> find pos
      | 1 -> pos
      | 2 -> find pos + relative_base
      | _ -> failwith (Printf.sprintf "Invalid mode %d" mode)
    in
    let set key data = Map.set instructions ~key ~data in
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
    | 3 -> (
        let write_pos = param (ptr + 1) modes in
        match inputs with
        | [] ->
            NeedInput
              { instructions; ptr; relative_base; outputs = List.rev outputs }
        | input :: inputs ->
            let new_map = set write_pos input in
            execute_impl new_map (ptr + 2) relative_base inputs outputs)
    | 4 ->
        let output = param (ptr + 1) modes |> find in
        execute_impl instructions (ptr + 2) relative_base inputs
          (output :: outputs)
    | 5 | 6 ->
        let mode1 = modes % 10 in
        let input1 = param (ptr + 1) mode1 |> find in
        let condition =
          (opcode = 5 && not (input1 = 0)) || (opcode = 6 && input1 = 0)
        in
        let mode2 = modes / 10 % 10 in
        let input2 = param (ptr + 2) mode2 |> find in
        if condition then
          execute_impl instructions input2 relative_base inputs outputs
        else execute_impl instructions (ptr + 3) relative_base inputs outputs
    | 9 ->
        let input = param (ptr + 1) modes |> find in
        execute_impl instructions (ptr + 2) (relative_base + input) inputs
          outputs
    | 99 -> Terminated { outputs = List.rev outputs }
    | _ -> failwith (Printf.sprintf "Invalid opcode %d" opcode)
  in
  execute_impl instructions ptr relative_base inputs []

module Pair = struct
  type t = int * int [@@deriving compare, sexp_of]
end

module Lexicographical_order = struct
  include Pair
  include Comparator.Make (Pair)
end

let () =
  let instructions =
    In_channel.create "input.txt"
    |> In_channel.input_lines |> List.hd_exn |> String.split ~on:','
    |> List.map ~f:Int.of_string
    |> List.mapi ~f:(fun i instruction -> (i, instruction))
    |> Map.of_alist_exn (module Int)
  in
  let outputs =
    match execute instructions 0 0 [] with
    | Terminated { outputs } -> outputs
    | _ -> failwith "Invalid result"
  in
  let drawings = List.groupi outputs ~break:(fun i _ _ -> i % 3 = 0) in
  let () =
    List.iter drawings ~f:(fun drawing -> assert (List.length drawing = 3))
  in
  let final_screen =
    let empty_screen = Map.empty (module Lexicographical_order) in
    let draw screen drawing =
      let x = List.nth_exn drawing 0 in
      let y = List.nth_exn drawing 1 in
      let tile = List.nth_exn drawing 2 in
      Map.set screen ~key:(x, y) ~data:tile
    in
    List.fold drawings ~init:empty_screen ~f:draw
  in
  let answer =
    Map.keys final_screen
    |> List.filter ~f:(fun key -> Map.find_exn final_screen key = 2)
    |> List.length
  in
  printf "%d\n" answer
