open Base
open Stdio

let grid =
  In_channel.create "input.txt"
  |> In_channel.input_lines
  |> List.map ~f:String.to_array
  |> Array.of_list |> Array.transpose_exn

let is_valid_x x = 0 <= x && x < Array.length grid
let is_valid_y y = 0 <= y && y < Array.length grid.(0)

let gcd a b =
  let rec gcd_impl a b = if b = 0 then a else gcd_impl b (a % b) in
  gcd_impl (Int.abs a) (Int.abs b)

let most_asteroids_loc =
  let count_asteroids srcx srcy =
    let count_asteroids_on_line delx dely =
      let rec traverse x y =
        if not (is_valid_x x) then 0
        else if not (is_valid_y y) then 0
        else if Char.(grid.(x).(y) = '#') then 1
        else traverse (x + delx) (y - dely)
      in
      traverse (srcx + delx) (srcy - dely)
    in
    Array.foldi grid ~init:0 ~f:(fun x count row ->
        Array.foldi row ~init:count ~f:(fun y count _ ->
            if x = srcx && y = srcy then count
            else if gcd (x - srcx) (srcy - y) <> 1 then count
            else count + count_asteroids_on_line (x - srcx) (srcy - y)))
  in
  Array.foldi grid ~init:(None, -1) ~f:(fun x (loc, best) row ->
      Array.foldi row ~init:(loc, best) ~f:(fun y (loc, best) cell ->
          if Char.(cell <> '#') then (loc, best)
          else
            let count = count_asteroids x y in
            if count > best then (Some (x, y), count) else (loc, best)))
  |> fst |> Option.value_exn

let find_nth_vaporized srcx srcy n =
  let deltas =
    Array.foldi grid ~init:[] ~f:(fun x deltas row ->
        Array.foldi row ~init:deltas ~f:(fun y deltas _ ->
            if x = srcx && y = srcy then deltas
            else if gcd (x - srcx) (srcy - y) <> 1 then deltas
            else (x - srcx, srcy - y) :: deltas))
  in
  let sorted_deltas =
    let compare (delx1, dely1) (delx2, dely2) =
      let quad (x, y) =
        if x >= 0 && y > 0 then 0
        else if y <= 0 && x > 0 then 1
        else if x <= 0 && y < 0 then 2
        else if y >= 0 && x < 0 then 3
        else 0
      in
      let quad1 = quad (delx1, dely1) in
      let quad2 = quad (delx2, dely2) in
      if quad1 <> quad2 then quad1 - quad2
      else
        let len1 = (delx1 * delx1) + (dely1 * dely1) in
        let len2 = (delx2 * delx2) + (dely2 * dely2) in
        let discriminant = (dely1 * dely1 * len2) - (len1 * dely2 * dely2) in
        match quad1 with
        | 0 | 2 -> -discriminant
        | 1 | 3 -> discriminant
        | _ -> failwith (Printf.sprintf "Invalid quadrant: %d" quad1)
    in
    List.sort deltas ~compare
  in
  let asteroids_on_line delx dely =
    let rec traverse x y asteroids =
      if not (is_valid_x x) then asteroids
      else if not (is_valid_y y) then asteroids
      else
        let new_asteroids =
          if Char.(grid.(x).(y) = '#') then (x, y) :: asteroids else asteroids
        in
        traverse (x + delx) (y - dely) new_asteroids
    in
    traverse (srcx + delx) (srcy - dely) [] |> List.rev
  in
  (* List.iter sorted_deltas ~f:(fun (x, y) -> *)
  (*     printf "delta: %d,%d " x y; *)
  (*     List.iter (asteroids_on_line x y) ~f:(fun (x, y) -> printf "%d,%d " x y); *)
  (*     printf "\n"); *)
  let asteroids =
    List.map sorted_deltas ~f:(fun (x, y) -> asteroids_on_line x y)
  in
  let rec find_nth_vaporized_impl seen not_seen n =
    match not_seen with
    | [] -> find_nth_vaporized_impl [] (List.rev seen) n
    | not_seen_hd :: not_seen_tl -> (
        match not_seen_hd with
        | [] -> find_nth_vaporized_impl seen not_seen_tl n
        | hd :: tl ->
            if n = 1 then hd
            else find_nth_vaporized_impl (tl :: seen) not_seen_tl (n - 1))
  in
  find_nth_vaporized_impl [] asteroids n

let () =
  let srcx, srcy = most_asteroids_loc in
  printf "%d,%d\n" srcx srcy;
  let nthx, nthy = find_nth_vaporized srcx srcy 200 in
  printf "%d,%d\n" nthx nthy
