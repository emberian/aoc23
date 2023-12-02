open Core

type color = Red | Green | Blue
type constraints = color -> int option
type round = (color * int) list
type game = int * round list

let round_satisfies_constraints round (constrs : constraints) : bool =
  List.for_all round ~f:(fun (color, count) ->
      match constrs color with Some max -> count <= max | None -> true)

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
                  | l -> failwithf "bad: %s" (Sexp.to_string (sexp_of_list sexp_of_string l)) ()))
  in
  (roundnum, contents)

let read_input path =
  In_channel.with_file path ~f:(fun file ->
      In_channel.fold_lines file ~init:0 ~f:(fun acc line ->
          let roundnum, contents = parse_game line in
          if
            List.for_all contents ~f:(fun round ->
                round_satisfies_constraints round (function
                  | Red -> Some 12
                  | Green -> Some 13
                  | Blue -> Some 14))
          then acc + roundnum
          else acc))

let go () = printf "%d\n" (read_input "input.txt")
