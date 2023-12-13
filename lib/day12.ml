open Core

type slot = Wildcard | On | Off [@@deriving compare, equal, sexp, hash]
type puzzle = { line : slot list; contig_spans : int list }

let slot_of_char = function
  | '?' -> Wildcard
  | '#' -> On
  | '.' -> Off
  | _ -> failwith "bad char"

module Memo = Hashtbl.Make (struct
  type t = slot list * int list [@@deriving compare, equal, sexp, hash]
end)

let memo : int Memo.t = Memo.create ()

let rec count (line : slot list) (spans : int list) =
  let skip_offs line spans =
    match (line, spans) with
    | [], [] -> 1
    | Wildcard :: rest, sp | Off :: rest, sp -> count rest sp
    | _ -> 0
  in
  let rec drop_ons line spans =
    match (line, spans) with
    | [], 0 :: rs -> count line rs
    | Wildcard :: rs, 0 :: bs | Off :: rs, 0 :: bs -> count rs bs
    | Wildcard :: rs, l :: ls | On :: rs, l :: ls -> drop_ons rs ((l - 1) :: ls)
    | _ -> 0
  in
  match Hashtbl.find memo (line, spans) with
  | Some x -> x
  | None ->
      let total = skip_offs line spans + drop_ons line spans in
      Hashtbl.set memo ~key:(line, spans) ~data:total;
      total

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
         let five sep s =
           String.concat ~sep:(Char.to_string sep) [ s; s; s; s; s ]
         in
         let spans =
           String.split ~on:',' (five ',' spans) |> List.map ~f:Int.of_string
         in
         let line =
           five '?' line |> String.to_list |> List.map ~f:slot_of_char
         in
         count line spans)
