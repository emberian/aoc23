open Core

let subst s =
  let s = String.substr_replace_all s ~pattern:"one" ~with_:"o1e" in
  let s = String.substr_replace_all s ~pattern:"two" ~with_:"t2o" in
  let s = String.substr_replace_all s ~pattern:"three" ~with_:"t3ree" in
  let s = String.substr_replace_all s ~pattern:"four" ~with_:"4our" in
  let s = String.substr_replace_all s ~pattern:"five" ~with_:"5ive" in
  let s = String.substr_replace_all s ~pattern:"six" ~with_:"6ix" in
  let s = String.substr_replace_all s ~pattern:"seven" ~with_:"7even" in
  let s = String.substr_replace_all s ~pattern:"eight" ~with_:"e8ght" in
  let s = String.substr_replace_all s ~pattern:"nine" ~with_:"9ine" in
  s

let calibration_values (s : string) : int * int =
  match
    String.fold (subst s) ~init:(None, None) ~f:(fun (first, last) c ->
        if Char.is_digit c then
          let n = Char.to_int c - Char.to_int '0' in
          match (first, last) with
          | None, None -> (Some n, None)
          | Some first, None -> (Some first, Some n)
          | Some first, Some _ -> (Some first, Some n)
          | None, Some _ -> failwith "impossible"
        else (first, last))
  with
  | None, None -> failwith "no digits"
  | Some first, None -> (first, first)
  | Some first, Some last -> (first, last)
  | _ -> failwith "impossible"

let parse_file path =
  In_channel.with_file path ~f:(fun file ->
      In_channel.fold_lines file ~init:0 ~f:(fun acc line ->
          let first, last = calibration_values line in
          acc + ((first * 10) + last)))

let go () =
  let result = parse_file "input.txt" in
  printf "%d\n" result
