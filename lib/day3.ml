open Core

type pos = { row : int; col : int }
type grid = string list

let is_symbol c =
  match c with
  | '$' | '/' | '&' | '=' | '+' | '%' | '@' | '#' | '-' | '*' -> true
  | _ -> false

let char_at (g : grid) (p : pos) = String.get (List.nth_exn g p.row) p.col

let neighborhood_around (g : grid) (p : pos) : (char * pos) list =
  let offsets = [ -1; 0; 1 ] in
  let neighbors = List.cartesian_product offsets offsets in
  let neighbors = List.filter neighbors ~f:(fun (x, y) -> x <> 0 || y <> 0) in
  let neighbors =
    List.map neighbors ~f:(fun (x, y) -> { row = p.row + x; col = p.col + y })
  in
  let neighbors =
    List.filter neighbors ~f:(fun { row; col } ->
        row >= 0
        && row < List.length g
        && col >= 0
        && col < String.length (List.nth_exn g 0))
  in
  List.map neighbors ~f:(fun p -> (char_at g p, p))

let has_symbol_near g p =
  List.exists (neighborhood_around g p) ~f:(fun (c, _) -> is_symbol c)

let is_partnum (g : grid) (start : pos) : bool =
  let maxcol = String.length (List.nth_exn g 0) in
  let rec loop ix =
    if ix.col >= maxcol then Some false
    else if not (Char.is_digit (char_at g ix)) then None
    else if has_symbol_near g ix then Some true
    else loop { row = ix.row; col = ix.col + 1 }
  in
  loop start |> Option.value ~default:false

type potential_partnum = pos * int

let digit_re = Re2.create_exn "\\d+"

let scan_line (g : grid) (row : int) (l : string) : int =
  let matches = Re2.get_matches_exn digit_re l in
  List.fold matches ~init:0 ~f:(fun numsum m ->
      let offset, _ = Re2.Match.get_pos_exn ~sub:(`Index 0) m in
      let ostensible_partnum =
        Int.of_string (Re2.Match.get_exn ~sub:(`Index 0) m)
      in
      if is_partnum g { row; col = offset } then numsum + ostensible_partnum
      else numsum)

let grid_foldi g ~init ~f =
  let rec loop row col acc =
    if row >= List.length g then acc
    else if col >= String.length (List.nth_exn g 0) then loop (row + 1) 0 acc
    else loop row (col + 1) (f { row; col } (char_at g { row; col }) acc)
  in
  loop 0 0 init

let sum_gear_ratios (g : grid) : int =
  let potential_gears =
    grid_foldi g ~init:[] ~f:(fun pos c acc ->
        if Char.equal c '*' then
          let _nearby_number_pos =
            neighborhood_around g pos
            |> List.filter ~f:(fun (c, _) -> Char.is_digit c)
          in
          pos :: acc
        else acc)
  in
  (List.nth_exn potential_gears 0).row

let go () =
  let contents = In_channel.read_lines "input.txt" in
  let allsum =
    List.foldi contents ~init:0 ~f:(fun i acc l -> acc + scan_line contents i l)
  in
  printf "%d\n" allsum
