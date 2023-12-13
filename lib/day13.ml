open Core

type digest = int list [@@deriving sexp]

type board = {
  matrix : char list list;
  rows_digest : digest;
  cols_digest : digest;
}
[@@deriving sexp]

let find_symmetry_in_digest (d : digest) : int option =
  (* detects if a digest is symmetrical around one of its indices *)
  let rec symmetry_at_point (d : digest) (ix : int) flipped =
    if ix > List.length d / 2 then
      symmetry_at_point (List.rev d) (List.length d - ix) true
    else if ix = 0 then (false, 0)
    else
      let first_portion, rest = List.split_n d ix in
      let second_portion, straggler = List.split_n rest ix in
      (List.equal Int.equal first_portion (List.rev second_portion), ix)
  in
  List.find_mapi d ~f:(fun ix _ ->
      match symmetry_at_point d ix false with
      | true, ix -> Some (ix + 1)
      | false, _ -> None)

let render_board (b : char list list) = function
  | Some len ->
      let first_half, second_half = List.split_n b len in
      let first_half_str =
        List.map first_half ~f:String.of_char_list |> String.concat ~sep:"\n"
      in
      let second_half_str =
        List.map second_half ~f:String.of_char_list |> String.concat ~sep:"\n"
      in
      Stdio.printf "Split board:\n%s\n\n%s\n" first_half_str second_half_str
  | None -> ()

let score_board (b : board) : int =
  (* detects if a board is symmetrical around one of its indices either vertically or horizontally *)
  let vert = find_symmetry_in_digest b.cols_digest in
  let horz = find_symmetry_in_digest b.rows_digest in
  print_s [%message (vert : int option) (horz : int option)];
  render_board (List.transpose_exn b.matrix) vert;
  render_board b.matrix horz;
  Option.value vert ~default:0 + (100 * Option.value horz ~default:0)

let pack_data (s : char list) : int =
  List.fold s ~init:0 ~f:(fun acc c ->
      let bit = match c with '#' -> 1 | '.' -> 0 | _ -> failwith "fmt" in
      (acc lsl 1) lor bit)

let read_input () =
  let input = In_channel.read_lines "input.txt" in
  let parse_board (lines : string list) : board =
    let matrix = List.map lines ~f:String.to_list in
    let rows_digest = List.map matrix ~f:pack_data in
    let cols_digest = List.map (List.transpose_exn matrix) ~f:pack_data in
    { matrix; rows_digest; cols_digest }
  in
  let rec scan_board lines acc =
    match lines with
    | [] -> (parse_board (List.rev acc), [])
    | "" :: rest -> (parse_board (List.rev acc), rest)
    | line :: rest -> scan_board rest (line :: acc)
  in
  let rec scan_boards lines acc =
    match lines with
    | [] -> List.rev acc
    | _ ->
        let board, rest = scan_board lines [] in
        scan_boards rest (board :: acc)
  in
  scan_boards input []

let go () =
  let boards = read_input () in
  List.sum (module Int) boards ~f:score_board |> Stdio.printf "%d\n"
