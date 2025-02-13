open Base
open Stdio

let grid =
  In_channel.create "input.txt"
  |> In_channel.input_lines
  |> List.map ~f:String.to_array
  |> Array.of_list

let gcd a b =
  let rec gcd_impl a b = if b = 0 then a else gcd_impl b (a % b) in
  gcd_impl (Int.abs a) (Int.abs b)

let most_asteroids =
  let asteroids srcx srcy =
    let asteroids_on_line delx dely =
      let is_valid_x x = 0 <= x && x < Array.length grid in
      let is_valid_y y = 0 <= y && y < Array.length grid.(0) in
      let rec traverse x y =
        if not (is_valid_x x) then 0
        else if not (is_valid_y y) then 0
        else if Char.(grid.(x).(y) = '#') then 1
        else traverse (x + delx) (y + dely)
      in
      traverse (srcx + delx) (srcy + dely)
    in
    Array.foldi grid ~init:0 ~f:(fun x count row ->
        Array.foldi row ~init:count ~f:(fun y count _ ->
            if x = srcx && y = srcy then count
            else if gcd (x - srcx) (y - srcy) <> 1 then count
            else count + asteroids_on_line (x - srcx) (y - srcy)))
  in
  Array.foldi grid ~init:0 ~f:(fun x best row ->
      Array.foldi row ~init:best ~f:(fun y best cell ->
          if Char.(cell <> '#') then best else asteroids x y |> Int.max best))

let () = printf "%d\n" most_asteroids
