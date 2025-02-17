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

type dir = Up | Down | Left | Right

let turn_left dir =
  match dir with Up -> Left | Down -> Right | Left -> Down | Right -> Up

let turn_right dir = dir |> turn_left |> turn_left |> turn_left

let move_in_dir dir (x, y) =
  match dir with
  | Up -> (x - 1, y)
  | Down -> (x + 1, y)
  | Left -> (x, y - 1)
  | Right -> (x, y + 1)

let rec traverse grid dir pos instructions ptr relative_base =
  let pos_color =
    match Map.find grid pos with Some pos_color -> pos_color | None -> 0
  in
  let result = execute instructions ptr relative_base [ pos_color ] in
  match result with
  | Terminated { outputs } -> (
      match outputs with
      | [ output1; _ ] -> Map.set grid ~key:pos ~data:output1
      | _ -> failwith "Invalid state")
  | NeedInput { instructions; ptr; relative_base; outputs } -> (
      match outputs with
      | [ output1; output2 ] ->
          let grid = Map.set grid ~key:pos ~data:output1 in
          let dir = if output2 = 0 then turn_left dir else turn_right dir in
          let pos = move_in_dir dir pos in
          traverse grid dir pos instructions ptr relative_base
      | _ -> failwith "Invalid state")

let instructions =
  In_channel.create "input.txt"
  |> In_channel.input_lines |> List.hd_exn |> String.split ~on:','
  |> List.map ~f:Int.of_string
  |> List.mapi ~f:(fun i instruction -> (i, instruction))
  |> Map.of_alist_exn (module Int)

module Pair = struct
  type t = int * int [@@deriving compare, sexp_of]
end

module Lexicographical_order = struct
  include Pair
  include Comparator.Make (Pair)
end

let () =
  let grid =
    traverse
      (Map.empty (module Lexicographical_order))
      Up (0, 0) instructions 0 0
  in
  printf "%d" (Map.length grid)
