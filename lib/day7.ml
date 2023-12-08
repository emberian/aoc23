open Core

module Card = struct
  type t = A | K | Q | T | N9 | N8 | N7 | N6 | N5 | N4 | N3 | N2 | J
  [@@deriving compare, equal, hash, sexp]

  let of_char = function
    | 'A' -> A
    | 'K' -> K
    | 'Q' -> Q
    | 'J' -> J
    | 'T' -> T
    | '9' -> N9
    | '8' -> N8
    | '7' -> N7
    | '6' -> N6
    | '5' -> N5
    | '4' -> N4
    | '3' -> N3
    | '2' -> N2
    | _ -> failwith "Invalid card"
end

module Multiplicity = struct
  type t = (Card.t, int) Hashtbl.t

  let create () = Hashtbl.create (module Card)

  let add_card t card =
    match Hashtbl.find t card with
    | None -> Hashtbl.set t ~key:card ~data:1
    | Some n -> Hashtbl.set t ~key:card ~data:(n + 1)

  let to_list t =
    List.sort
      (Hashtbl.fold t ~init:[] ~f:(fun ~key ~data acc -> (key, data) :: acc))
      ~compare:(fun (_, n1) (_, n2) -> Int.compare n2 n1)
end

(* module Multiplicity = Hash_map.Make (Card) *)

type handkind =
  | Five_of_a_kind
  | Four_of_a_kind
  | Full_house
  | Three_of_a_kind
  | Two_pair
  | Pair
  | High_card
[@@deriving compare, equal]

let detect_hands cards =
  let multiplicity = Multiplicity.create () in
  List.iter cards ~f:(fun card -> Multiplicity.add_card multiplicity card);
  let multiplicities = Multiplicity.to_list multiplicity in
  match multiplicities with
  | [ (_, 5) ] -> Five_of_a_kind
  | [ (Card.J, 4); (_, 1) ] -> Five_of_a_kind
  | [ (_, 4); (Card.J, 1) ] -> Five_of_a_kind
  | [ (_, 4); (_, 1) ] -> Four_of_a_kind
  | [ (Card.J, 3); (_, 2) ] -> Five_of_a_kind
  | [ (_, 3); (Card.J, 2) ] -> Five_of_a_kind
  | [ (_, 3); (_, 2) ] -> Full_house
  | [ (Card.J, 3); (_, 1); (_, 1) ] -> Four_of_a_kind
  | [ (_, 3); (Card.J, 1); (_, 1) ] -> Four_of_a_kind
  | [ (_, 3); (_, 1); (Card.J, 1) ] -> Four_of_a_kind
  | [ (_, 3); (_, 1); (_, 1) ] -> Three_of_a_kind
  | [ (Card.J, 2); (_, 2); (_, 1) ] -> Four_of_a_kind
  | [ (_, 2); (Card.J, 2); (_, 1) ] -> Four_of_a_kind
  | [ (_, 2); (_, 2); (Card.J, 1) ] -> Full_house
  | [ (_, 2); (_, 2); (_, 1) ] -> Two_pair
  | [ (Card.J, 2); (_, 1); (_, 1); (_, 1) ] -> Three_of_a_kind
  | [ (_, 2); (Card.J, 1); (_, 1); (_, 1) ] -> Three_of_a_kind
  | [ (_, 2); (_, 1); (Card.J, 1); (_, 1) ] -> Three_of_a_kind
  | [ (_, 2); (_, 1); (_, 1); (Card.J, 1) ] -> Three_of_a_kind
  | [ (_, 2); (_, 1); (_, 1); (_, 1) ] -> Pair
  | [ (Card.J, 1); (_, 1); (_, 1); (_, 1); (_, 1) ] -> Pair
  | [ (_, 1); (Card.J, 1); (_, 1); (_, 1); (_, 1) ] -> Pair
  | [ (_, 1); (_, 1); (Card.J, 1); (_, 1); (_, 1) ] -> Pair
  | [ (_, 1); (_, 1); (_, 1); (Card.J, 1); (_, 1) ] -> Pair
  | [ (_, 1); (_, 1); (_, 1); (_, 1); (Card.J, 1) ] -> Pair
  | [ (_, 1); (_, 1); (_, 1); (_, 1); (_, 1) ] -> High_card
  | multiplicities ->
      failwithf "Invalid hand: %s / %s"
        (List.sexp_of_t Card.sexp_of_t cards |> Sexp.to_string)
        (List.sexp_of_t (sexp_of_pair Card.sexp_of_t sexp_of_int) multiplicities
        |> Sexp.to_string)
        ()

let compare_hands (cards1 : Card.t list) (cards2 : Card.t list) =
  let hands1 = detect_hands cards1 in
  let hands2 = detect_hands cards2 in
  if equal_handkind hands1 hands2 then List.compare Card.compare cards1 cards2
  else compare_handkind hands1 hands2

let () = assert (compare_handkind Five_of_a_kind Four_of_a_kind < 0)

let go () =
  let lines = In_channel.read_lines "input.txt" in
  let hands =
    List.map lines ~f:(fun line ->
        let hand, bid = String.lsplit2_exn line ~on:' ' in
        (String.to_list hand |> List.map ~f:Card.of_char, bid))
  in
  let ranked =
    List.sort hands ~compare:(fun (h1, _) (h2, _) -> compare_hands h2 h1)
  in
  printf "%s\n"
    (Bigint.to_string
       (List.foldi ranked ~init:Bigint.zero ~f:(fun i acc (_, bid) ->
            let rank = i + 1 in
            let bid = Int.of_string bid in
            Bigint.(acc + (of_int rank * of_int bid)))))
