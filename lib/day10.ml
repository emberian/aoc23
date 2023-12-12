open Core

type node = {
  mutable connections : node list;
  mutable visited : bool;
  mutable dist_from_start : int;
  data : char;
}

type 'a grid = 'a array array
type pos = { row : int; col : int }

let neighborhood_around (g : node grid) (p : pos) : (node * pos) Sequence.t =
  let offsets = [ -1; 0; 1 ] in
  let neighbors =
    Sequence.(cartesian_product (of_list offsets) (of_list offsets))
  in
  let neighbors =
    Sequence.filter neighbors ~f:(fun (x, y) -> x <> 0 || y <> 0)
  in
  let neighbors =
    Sequence.map neighbors ~f:(fun (x, y) ->
        { row = p.row + x; col = p.col + y })
  in
  let neighbors =
    Sequence.filter neighbors ~f:(fun { row; col } ->
        row >= 0 && row < Array.length g && col >= 0 && col < Array.length g.(0))
  in
  Sequence.map neighbors ~f:(fun { row; col } -> (g.(row).(col), p))

let go () =
  let lines = In_channel.read_lines "input.txt" in
  let map : node grid =
    Array.make_matrix ~dimx:(List.length lines)
      ~dimy:(String.length (List.hd_exn lines))
      { connections = []; data = ' '; visited = false; dist_from_start = 0 }
  in
  List.iteri lines ~f:(fun y line ->
      String.iteri line ~f:(fun x c ->
          map.(y).(x) <- { (map.(y).(x)) with data = c }));
  Array.iteri map ~f:(fun rowix row ->
      Array.iteri row ~f:(fun colix tile ->
          let offsets =
            match tile.data with
            | '.' | 'S' -> []
            | '|' -> [ { row = -1; col = 0 }; { row = 1; col = 0 } ]
            | '-' -> [ { row = 0; col = -1 }; { row = 0; col = 1 } ]
            | 'L' -> [ { row = -1; col = 0 }; { row = 0; col = 1 } ]
            | 'J' -> [ { row = -1; col = 0 }; { row = 0; col = -1 } ]
            | '7' -> [ { row = 1; col = 0 }; { row = 0; col = -1 } ]
            | 'F' -> [ { row = 1; col = 0 }; { row = 0; col = 1 } ]
            | c -> failwithf "Invalid tile: %c" c ()
          in
          let neighbors =
            Sequence.(map @@ of_list offsets) ~f:(fun { row; col } ->
                { row = rowix + row; col = colix + col })
          in
          let neighbors =
            Sequence.filter neighbors ~f:(fun { row; col } ->
                row >= 0
                && row < Array.length map
                && col >= 0
                && col < Array.length map.(0))
          in
          Sequence.iter neighbors ~f:(fun { row; col } ->
              map.(row).(col).connections <- tile :: map.(row).(col).connections)));
  let animal_pos, animal_node =
    Array.find_mapi map ~f:(fun rowix row ->
        Array.find_mapi row ~f:(fun colix tile ->
            if Char.equal tile.data 'S' then
              Some ({ row = rowix; col = colix }, tile)
            else None))
    |> Option.value_exn
  in
  Sequence.iter (neighborhood_around map animal_pos) ~f:(fun (n, _) ->
      if
        List.exists n.connections ~f:(fun c ->
            List.mem c.connections animal_node ~equal:phys_same)
      then animal_node.connections <- n :: animal_node.connections);
  let queue = Queue.create () in
  Queue.enqueue queue animal_node;
  while not (Queue.is_empty queue) do
    let node = Queue.dequeue_exn queue in
    if not node.visited then (
      node.visited <- true;
      List.iter node.connections ~f:(fun n ->
          n.dist_from_start <- node.dist_from_start + 1;
          if not n.visited then Queue.enqueue queue n))
  done;
  (* Now find the element with the largest dist_from_start *)
  let max_dist =
    Array.fold map ~init:0 ~f:(fun acc row ->
        Array.fold row ~init:acc ~f:(fun acc tile ->
            if tile.dist_from_start > acc then tile.dist_from_start else acc))
  in
  printf "Distance from start: %d\n" (max_dist + 1)
