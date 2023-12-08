open Core

type race = { time : int; distance : int }

let distance_for_race (r : race) (charge_time : int) : int =
  let remaining_time = r.time - charge_time in
  let distance = charge_time * remaining_time in
  distance

let all_possible_distances (r : race) =
  Array.init r.time ~f:(distance_for_race r)

let count_wins (r : race) =
  all_possible_distances r |> Array.count ~f:(fun d -> d > r.distance)

let go () = 
  printf "%d\n" (count_wins { time = 46828479; distance = 347152214061471 })

let races =
  [
    { time = 46; distance = 347 };
    { time = 82; distance = 1522 };
    { time = 84; distance = 1406 };
    { time = 79; distance = 1471 };
  ]