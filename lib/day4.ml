open Core

type card = { id : int; winning : int list; ticket : int list }

let parse_card s =
  let id, rest = String.lsplit2_exn s ~on:':' in
  let id =
    String.strip id |> String.split ~on:' ' |> List.last_exn |> int_of_string
  in
  let winning, ticket = String.lsplit2_exn rest ~on:'|' in
  let winning =
    List.map ~f:int_of_string
      (String.split ~on:' ' (String.strip winning)
      |> List.filter ~f:(fun s -> not (String.is_empty s)))
  in
  let ticket =
    List.map ~f:int_of_string
      (String.split ~on:' ' (String.strip ticket)
      |> List.filter ~f:(fun s -> not (String.is_empty s)))
  in
  { id; winning; ticket }

let score_card (c : card) : int =
  List.filter c.ticket ~f:(fun i -> List.mem c.winning i ~equal:( = ))
  |> List.length

let score_deck (c : card array) : Bigint.t =
  let card_counts = Array.create ~len:(Array.length c) 1 in
  let inbounds i = min i (Array.length c - 1) in
  let rec loop i =
    if i = Array.length c then ()
    else
      let score = score_card c.(i) in
      if score <> 0 then
        for j = inbounds (i + 1) to inbounds (i + score) do
          card_counts.(j) <- card_counts.(i) + card_counts.(j)
        done;
      loop (i + 1)
  in
  loop 0;
  Array.sum (module Bigint) card_counts ~f:Bigint.of_int

let go () =
  let cards =
    List.map ~f:parse_card (In_channel.read_lines "input.txt") |> Array.of_list
  in
  printf "%s\n" (score_deck cards |> Bigint.to_string)
