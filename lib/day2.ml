open Core

type color = Red | Green | Blue [@@deriving eq]
type constraints = color -> int option
type round = (color * int) list
type game = int * round list

let minumum_cubes_for_round (round : round) (c : color) : int =
  Option.value ~default:0
    (List.fold round ~init:None ~f:(fun acc (color, count) ->
         if phys_equal c color then
           match acc with
           | Some prevcount -> Some (max count prevcount)
           | None -> Some count
         else acc))

let minimum_cubes_for_game ((_, rounds) : game) : color -> int =
  let roundmins = List.map rounds ~f:minumum_cubes_for_round in
  fun c ->
    let mins_for_color = List.map roundmins ~f:(fun f -> f c) in
    printf "mins_for_color: %s\n"
      (Sexp.to_string (sexp_of_list sexp_of_int mins_for_color));
    List.max_elt mins_for_color ~compare:Int.compare |> Option.value_exn

let parse_game line : game =
  (* Parse a line like "Game 0: 3 red, 2 green; 2 blue, 2 green, 4 red" *)
  let roundnum =
    String.split line ~on:':' |> List.hd_exn |> String.split ~on:' '
    |> List.last_exn |> Int.of_string
  in
  let contents =
    String.slice line (String.index_exn line ':' + 1) (String.length line)
    |> String.split ~on:';'
    |> List.map ~f:(fun round ->
           String.split ~on:',' (String.strip round)
           |> List.map ~f:(fun cube ->
                  match String.split ~on:' ' (String.strip cube) with
                  | [ count; color ] ->
                      ( (match String.strip color with
                        | "red" -> Red
                        | "green" -> Green
                        | "blue" -> Blue
                        | _ -> failwith "bad color"),
                        Int.of_string (String.strip count) )
                  | l ->
                      failwithf "bad: %s"
                        (Sexp.to_string (sexp_of_list sexp_of_string l))
                        ()))
  in
  (roundnum, contents)

let read_input path =
  In_channel.with_file path ~f:(fun file ->
      In_channel.fold_lines file ~init:0 ~f:(fun acc line ->
          let game = parse_game line in
          let min_cubes = minimum_cubes_for_game game in
          printf "Game (%s): R %d G %d B %d\n" line (min_cubes Red)
            (min_cubes Green) (min_cubes Blue);
          acc + (min_cubes Red * min_cubes Green * min_cubes Blue)))

let go () = printf "%d\n" (read_input "input.txt")
