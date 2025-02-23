open Base
open Stdio

let simulate moons =
  let apply_gravity (pos, vel) =
    let delta a b =
      let sign = Int.sign (Int.compare a b) in
      match sign with Zero -> 0 | Neg -> 1 | Pos -> -1
    in
    let new_vel =
      List.fold moons ~init:vel ~f:(fun vel (other_pos, _) ->
          vel + delta pos other_pos)
    in
    (pos, new_vel)
  in
  let apply_velocity (pos, vel) = (pos + vel, vel) in
  let moons = List.map moons ~f:apply_gravity in
  let moons = List.map moons ~f:apply_velocity in
  moons

module PairList = struct
  type t = (int * int) list [@@deriving compare, sexp_of]
end

module Lexicographical_order = struct
  include PairList
  include Comparator.Make (PairList)
end

let find_period moons =
  let rec find_period_impl visited counter moons =
    match Map.find visited moons with
    | None ->
        let visited = Map.set visited ~key:moons ~data:counter in
        find_period_impl visited (counter + 1) (simulate moons)
    | Some x -> (x, counter - x)
  in
  find_period_impl (Map.empty (module Lexicographical_order)) 0 moons

let rec gcd a b = if b = 0 then a else gcd b (a % b)
let lcm a b = a / gcd a b * b

let () =
  let positions =
    In_channel.create "input.txt"
    |> In_channel.input_lines
    |> List.map ~f:(String.split ~on:' ')
    |> List.map ~f:(List.map ~f:Int.of_string)
  in
  let positions_by_coord = List.transpose_exn positions in
  let moons_by_coord =
    List.map positions_by_coord ~f:(List.map ~f:(fun pos -> (pos, 0)))
  in
  let periods = List.map moons_by_coord ~f:find_period in
  let () = List.iter periods ~f:(fun (x, _) -> assert (x = 0)) in
  let periods = List.map periods ~f:snd in
  let answer = List.fold periods ~init:1 ~f:lcm in
  printf "%d\n" answer
