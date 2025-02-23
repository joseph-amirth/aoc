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

let rec simulate_n n moons =
  if n = 0 then moons else simulate_n (n - 1) (simulate moons)

let total_energy moon =
  let positions = List.map moon ~f:fst in
  let velocities = List.map moon ~f:snd in
  let potential_energy =
    positions |> List.map ~f:Int.abs |> List.fold ~init:0 ~f:( + )
  in
  let kinetic_energy =
    velocities |> List.map ~f:Int.abs |> List.fold ~init:0 ~f:( + )
  in
  potential_energy * kinetic_energy

let () =
  let positions =
    In_channel.create "sample2.txt"
    |> In_channel.input_lines
    |> List.map ~f:(String.split ~on:' ')
    |> List.map ~f:(List.map ~f:Int.of_string)
  in
  let positions_by_coord = List.transpose_exn positions in
  let moons_by_coord =
    List.map positions_by_coord ~f:(List.map ~f:(fun pos -> (pos, 0)))
  in
  let final_moons_by_coord = List.map moons_by_coord ~f:(simulate_n 100) in
  let final_moons = List.transpose_exn final_moons_by_coord in
  let final_moon_energies = List.map final_moons ~f:total_energy in
  let answer = List.fold final_moon_energies ~init:0 ~f:( + ) in
  printf "%d\n" answer
