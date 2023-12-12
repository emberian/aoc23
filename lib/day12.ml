open Core

type slot = Wildcard | On | Off [@@deriving compare, equal, sexp]
type puzzle = { line : slot list; contig_spans : int list }

let rec fits_span line spans =
  match (line, spans) with
  | On :: l, n :: ns -> if n = 0 then false else fits_span l ((n - 1) :: ns)
  | Off :: l, 0 :: ns -> fits_span l ns
  | Off :: l, ns ->
      fits_span (List.drop_while l ~f:(fun x -> phys_equal x Off)) ns
  | [], [] -> true
  | [], [ 0 ] -> true
  | _ -> false

(* This is a single-axis picross puzzle,
   where we need to count the number of solutions
   for the single line, such that counting the contiguous span
   lengths gives the given list *)

(* Example line: *)
(* ??#?#?#????.????#? 6,2,1,1 *)
let go () =
  let lines = In_channel.read_lines "input.txt" in
  printf "%d\n"
  @@ List.sum
       (module Int)
       lines
       ~f:(fun line ->
         let line, spans = String.lsplit2_exn line ~on:' ' in
         let spans = String.split ~on:',' spans |> List.map ~f:Int.of_string in
         let line =
           String.to_list line
           |> List.map ~f:(function
                | '?' -> Choice.of_list [ On; Off ]
                | '#' -> Choice.return On
                | '.' -> Choice.return Off
                | _ -> failwith "bad input")
         in
         (* Convert slot Choice.t list into slot list Choice.t *)
         let rec sequence choices =
           List.fold_right choices
             ~f:(fun choice acc ->
               Choice.bind (fun x -> Choice.map (fun xs -> x :: xs) acc) choice)
             ~init:(Choice.return [])
         in
         (* Count the number of solutions *)
         Choice.(count @@ filter (fun s -> fits_span s spans) (sequence line)))
