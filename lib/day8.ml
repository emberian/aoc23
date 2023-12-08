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

let least_common_multiple (l : Z.t list) = List.fold_left ~f:Z.lcm ~init:Z.one l

let go () =
  let input = In_channel.read_lines "input.txt" in
  let path =
    List.nth_exn input 0 |> String.to_list |> Sequence.cycle_list_exn
  in
  let g = Graph.create () in
  let q = Queue.create () in
  let finished = Queue.create () in
  List.iter (List.drop input 2) ~f:(parse_line g);
  Hashtbl.iter_keys g ~f:(fun k ->
      if String.is_suffix k ~suffix:"A" then Queue.enqueue q (k, path, 0));
  while not (Queue.is_empty q) do
    let cur_node, path_seq, local_ct = Queue.dequeue_exn q in
    if String.is_suffix cur_node ~suffix:"Z" then
      Queue.enqueue finished local_ct
    else
      let next_node, tl = Sequence.next path_seq |> Option.value_exn in
      let l, r = Hashtbl.find_exn g cur_node in
      let next_node = if Char.equal next_node 'L' then l else r in
      Queue.enqueue q (next_node, tl, local_ct + 1)
  done;
  printf "%s\n"
    (Z.to_string
       (least_common_multiple (Queue.to_list finished |> List.map ~f:Z.of_int)))
