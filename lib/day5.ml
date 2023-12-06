open Core

type _ index = | Ix : Bigint.t -> 'a index

let gi (Ix i) = i
let mapi (Ix i) ~f = Ix (f i)
let equali (Ix a : 'a index) (Ix b : 'a index) = Bigint.equal a b
let offi (Ix i) n = Ix (Bigint.(+) i n)

type ('a, 'b) map_entry = {
  dest_start : 'b index;
  src_start : 'a index;
  range_length : Bigint.t;
}
let dest_end {dest_start; range_length; _} = mapi dest_start ~f:(Bigint.(+) range_length)

let parse_map_entry line =
  let parts = String.split line ~on:' ' in
  match parts with
  | [ dest_start; src_start; range_length ] ->
      {
        dest_start = Ix (Bigint.of_string dest_start);
        src_start = Ix (Bigint.of_string src_start);
        range_length = Bigint.of_string range_length;
      }
  | _ -> failwith "Invalid input format"

let sort_by_src entries =
  let arr = Array.of_list entries in
  Array.sort arr ~compare:(fun {src_start= Ix a_src_start; _ } {src_start= Ix b_src_start; _} -> Bigint.compare a_src_start b_src_start);
  arr

let entry_contains : type a b. (a, b) map_entry -> a index -> bool =
  fun {src_start= Ix src_start; range_length; _} (Ix src_number) ->
  Bigint.(src_start <= src_number && src_number < src_start + range_length)

let mapping : type a b. (a, b) map_entry array -> a index -> b index =
  fun m src_number ->
  let index =
    Array.binary_search
      ~compare:(fun {src_start= Ix src_start; _} key -> Bigint.compare src_start key)
      m `Last_less_than_or_equal_to (gi src_number)
  in
  match index with
  | None -> Ix (gi src_number)
  | Some idx ->
      let open Bigint in
      if entry_contains m.(idx) src_number then
        mapi m.(idx).dest_start ~f:(fun i -> i + (gi src_number - gi m.(idx).src_start))
      else Ix (gi src_number)

let parse_section (lines : string list) expected_name =
  assert (String.equal (List.hd_exn lines) expected_name);
  let lines, rest =
    List.split_while (List.tl_exn lines) ~f:(fun line ->
        not (String.is_empty line))
  in
  ( sort_by_src (List.map ~f:parse_map_entry lines),
    List.drop_while ~f:String.is_empty rest )

type all_maps = {
  seeds : ([`Seed] index * Bigint.t) array;
  seed_to_soil : ([`Seed], [`Soil]) map_entry array;
  soil_to_fertilizer : ([`Soil], [`Fertilizer]) map_entry array;
  fertilizer_to_water : ([`Fertilizer], [`Water]) map_entry array;
    water_to_light : ([`Water], [`Light]) map_entry array;
    light_to_temperature : ([`Light], [`Temperature]) map_entry array;
    temperature_to_humidity : ([`Temperature], [`Humidity]) map_entry array;
    humidity_to_location : ([`Humidity], [`Location]) map_entry array;
  }

let read_file (lines : string list) =
  let seeds, rem = (List.hd_exn lines, List.drop lines 2) in
  let seeds =
    Array.of_list (String.split (String.drop_prefix seeds 6) ~on:' ' |> List.map ~f:Bigint.of_string)
  in
  let seeds = Array.init (Array.length seeds / 2) ~f:(fun i -> (Ix seeds.(i), seeds.(i + 1))) in
  let seed_to_soil, rem = parse_section rem "seed-to-soil map:" in
  let soil_to_fertilizer, rem = parse_section rem "soil-to-fertilizer map:" in
  let fertilizer_to_water, rem = parse_section rem "fertilizer-to-water map:" in
  let water_to_light, rem = parse_section rem "water-to-light map:" in
  let light_to_temperature, rem =
    parse_section rem "light-to-temperature map:"
  in
  let temperature_to_humidity, rem =
    parse_section rem "temperature-to-humidity map:"
  in
  let humidity_to_location, rem =
    parse_section rem "humidity-to-location map:"
  in
  assert (List.is_empty rem);
  {
    seeds;
    seed_to_soil;
    soil_to_fertilizer;
    fertilizer_to_water;
    water_to_light;
    light_to_temperature;
    temperature_to_humidity;
    humidity_to_location;
  }

let seed_to_location (maps : all_maps) (seed : [`Seed] index) =
  let soil = mapping maps.seed_to_soil seed in
  let fertilizer = mapping maps.soil_to_fertilizer soil in
  let water = mapping maps.fertilizer_to_water fertilizer in
  let light = mapping maps.water_to_light water in
  let temperature = mapping maps.light_to_temperature light in
  let humidity = mapping maps.temperature_to_humidity temperature in
  let location = mapping maps.humidity_to_location humidity in
  location

let inject_seeds : ([`Seed] index * Bigint.t) array -> ([`Seed], [`Seed]) map_entry array =
  fun seeds ->
  Array.map seeds ~f:(fun (seed, len) -> { dest_start = seed; src_start = seed; range_length = len})

let compose_maps (m1 : ('a, 'b) map_entry array) (m2 : ('b, 'c) map_entry array) : ('a, 'c) map_entry array =
  let lift_entry : type a b c. (a, b) map_entry -> (a, c) map_entry list =
    fun e ->
    (* Check if this entry needs to be split *)
      if equali (mapping m1 (dest_end e)) (offi (mapping m1 e.dest_start) e.range_length) then
        [ { e with dest_start = mapping m1 e.dest_start; } ]
      else
    { e with dest_start = mapping m2 e.dest_start; }
  in

let go () =
  let all_maps = read_file (In_channel.read_lines "input.txt") in
  let locations = Array.map all_maps.seeds ~f:(seeds_to_location all_maps) in
  printf "%s\n"
    (Array.min_elt locations ~compare:Bigint.compare
    |> Option.value_exn |> Bigint.to_string)
