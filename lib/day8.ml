open Core
module Graph = Hashtbl.Make (String)

(*
  This is a graph problem. We can represent the maze as a graph where each node
  is a location in the maze and each edge is a portal.
*)
let parse_line g line =
  (* Parse AAA = (BBB, CCC) with regex *)
  let re = Re2.create_exn "^(\\w+) = \\((\\w+), (\\w+)\\)$" in
  let matches = Re2.find_submatches_exn re line in
  let src = Option.value_exn matches.(1) in
  let l = Option.value_exn matches.(2) in
  let r = Option.value_exn matches.(3) in
  Hashtbl.add g ~key:src ~data:(l, r) |> ignore

let go () =
  let input = In_channel.read_lines "input.txt" in
  let path =
    List.nth_exn input 0 |> String.to_list |> Sequence.cycle_list_exn
  in
  let ct = ref Bigint.zero in
  let g = Graph.create () in
  let q = ref [] in
  List.iter (List.drop input 2) ~f:(parse_line g);
  Hashtbl.iter_keys g ~f:(fun k ->
      if String.is_suffix k ~suffix:"A" then q := (k, path) :: !q);
  while
    not (List.for_all !q ~f:(fun (n, _) -> String.is_suffix n ~suffix:"Z"))
  do
    Bigint.incr ct;
    q :=
      List.map !q ~f:(fun (cur_node, path_seq) ->
          let next_node, tl = Sequence.next path_seq |> Option.value_exn in
          let l, r = Hashtbl.find_exn g cur_node in
          let next_node = if Char.equal next_node 'L' then l else r in
          (next_node, tl))
  done;
  printf "Number of steps: %s\n" (Bigint.to_string !ct)
