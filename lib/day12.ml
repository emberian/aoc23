open Core

type slot = Wildcard | On | Off [@@deriving compare, equal, sexp]
type puzzle = { line : slot list; contig_spans : int list }

let spans_from_line l : int list =
    match
      List.fold l ~init:(0, []) ~f:(fun (cur_ons, revl) slot ->
          match slot with
          | On -> (cur_ons + 1, revl)
          | Off -> if cur_ons <> 0 then (0, cur_ons :: revl) else (0, revl)
          | Wildcard -> failwith "wildcard not expanded yet?")
    with
    | 0, revp -> List.rev revp
    | cur_ons, revp -> List.rev (cur_ons :: revp)

let fits_span line spans =
  let rec go line spans =
    print_s [%message (line : slot list) (spans : int list)];
    match (line, spans) with
    | [], [ 0 ] -> true
    | [], _ -> false
    | _, [] -> false
    | Wildcard :: line, _ -> failwith "wildcard not expanded yet?"
    | On :: line, span :: spans ->
        if span = 0 then false else go line ((span - 1) :: spans)
    | Off :: line, 0 :: spans ->
        go (List.drop_while line ~f:(equal_slot Off)) spans
    | Off :: line, spans -> go (List.drop_while line ~f:(equal_slot Off)) spans
  in
  go line spans
(* This is a single-axis picross puzzle,
   where we need to count the number of solutions
   for the single line, such that counting the contiguous span
   lengths gives the given list *)

let rec sequence choices =
  List.fold_right choices
    ~f:(fun choice acc ->
      Choice.bind (fun x -> Choice.map (fun xs -> x :: xs) acc) choice)
    ~init:(Choice.return [])

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
         print_s [%message (line : string) (spans : int list)];
         let line =
           String.to_list line
           |> List.map ~f:(function
                | '?' -> Choice.of_list [ On; Off ]
                | '#' -> Choice.return On
                | '.' -> Choice.return Off
                | _ -> failwith "bad input")
         in
         (* Count the number of solutions *)
         Choice.(
           count
           @@ filter
                (fun s -> Core.List.equal equal_int (spans_from_line s) spans)
                (sequence line)))
