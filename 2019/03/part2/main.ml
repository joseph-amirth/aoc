let read_file file_name =
  let rec read_file_impl file_handle =
    let open Option in
    let maybe_line =
      try Some (input_line file_handle) with End_of_file -> None
    in
    match maybe_line with
    | Some line -> line :: read_file_impl file_handle
    | None -> []
  in
  read_file_impl (open_in file_name)

let input_lines = read_file "input.txt"

type dir = Up | Down | Left | Right

let delx dir = match dir with Up | Down -> 0 | Left -> -1 | Right -> 1
let dely dir = match dir with Up -> 1 | Down -> -1 | Left | Right -> 0

type vec = { dir : dir; dist : int }

let traverse vec (x, y) =
  (x + (delx vec.dir * vec.dist), y + (dely vec.dir * vec.dist))

let wires =
  let input_lists = List.map (String.split_on_char ',') input_lines in
  let parse_vec str =
    let dir =
      match String.get str 0 with
      | 'U' -> Up
      | 'D' -> Down
      | 'L' -> Left
      | 'R' -> Right
      | _ -> raise (Failure "Unknown dir")
    in
    let dist = String.sub str 1 (String.length str - 1) |> int_of_string in
    { dir; dist }
  in
  List.map (fun input_list -> List.map parse_vec input_list) input_lists

type line =
  | Horizontal of { dist : int; x1 : int; x2 : int; y : int }
  | Vertical of { dist : int; x : int; y1 : int; y2 : int }

let dist line =
  match line with
  | Horizontal { dist; _ } -> dist
  | Vertical { dist; _ } -> dist

(* let debug line = *)
(*   match line with *)
(*   | Horizontal { x1; x2; y } -> Printf.printf "Horizontal: %d %d %d\n" x1 x2 y *)
(*   | Vertical { x; y1; y2 } -> Printf.printf "Vertical: %d %d %d\n" x y1 y2 *)

let line_of_points dist (x1, y1) (x2, y2) =
  if x1 == x2 then Vertical { dist; x = x1; y1; y2 }
  else Horizontal { dist; x1; x2; y = y1 }

let is_point_on_line (x, y) line =
  let rec in_range (l, r) x =
    if l <= r then l <= x && x <= r else in_range (r, l) x
  in
  match line with
  | Horizontal { x1; x2; y = line_y; _ } -> in_range (x1, x2) x && y == line_y
  | Vertical { x = line_x; y1; y2; _ } -> x == line_x && in_range (y1, y2) y

let wire_paths =
  let traverse_wire wire =
    let rec traverse_impl cur dist wire =
      match wire with
      | [] -> []
      | hd :: tl ->
          let next = traverse hd cur in
          line_of_points dist cur next :: traverse_impl next (dist + hd.dist) tl
    in
    traverse_impl (0, 0) 0 wire
  in
  List.map traverse_wire wires

let wire_path1 = List.nth wire_paths 0
let wire_path2 = List.nth wire_paths 1

let answer =
  let maybe_solve line1 line2 =
    match (line1, line2) with
    | Vertical { x; y1; _ }, Horizontal { y; x1; _ }
    | Horizontal { y; x1; _ }, Vertical { x; y1; _ } ->
        if is_point_on_line (x, y) line1 && is_point_on_line (x, y) line2 then
          Some (dist line1 + dist line2 + Int.abs (x - x1) + Int.abs (y - y1))
        else None
    | _ -> None
  in
  List.map
    (fun line1 ->
      List.filter_map (fun line2 -> maybe_solve line1 line2) wire_path2)
    wire_path1
  |> List.concat
  |> List.filter (fun dist -> dist != 0)
  |> List.fold_left Int.min Int.max_int

let () = answer |> string_of_int |> print_endline
