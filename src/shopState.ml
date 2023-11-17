open Card
open Player
open Random
open UsefulFunctions

exception InvalidPurchase of string
exception CardRemoval of string
exception NotEnough of string

type t = {
  cards : string list;
  player : Player.t;
  card_removal : int;
  removal_cost : int;
}

let removal_price = ref 3

let rec get_random_cards (cards : string list) (size : int) =
  match size with
  | 0 -> []
  | _ ->
      let element = Random.int (List.length cards) |> List.nth cards in
      element :: get_random_cards cards (size - 1)

let generate_shop_cards =
  let size = Random.int 2 + 5 in
  function
  | 1 -> get_random_cards t1_cards size
  | 2 -> get_random_cards t2_cards size
  | 3 -> get_random_cards t3_cards size
  | _ -> []

let create_shop (player : Player.t) =
  let floor = player |> player_stage |> fst in
  {
    cards = floor |> generate_shop_cards;
    player;
    card_removal = 1;
    removal_cost = !removal_price;
  }

let get_cards (shop : t) = shop.cards
let get_card_removals (shop : t) = shop.card_removal
let get_removal_cost (shop : t) = shop.removal_cost

let buy_card (shop : t) (card : string) =
  match
    (List.mem card shop.cards, player_gold shop.player - get_value card >= 0)
  with
  | false, _ ->
      raise (InvalidPurchase "The shopkeeper isn't selling that card.")
  | true, false ->
      raise
        (NotEnough
           "You checked your pockets and realized that you do not have enough \
            coins.")
  | true, true ->
      {
        shop with
        cards = remove_card shop.cards card;
        player =
          change_gold_player (add_card shop.player card) ~-(get_value card);
      }

let buy_card_removal (shop : t) (card : string) =
  match (shop.card_removal, player_gold shop.player - !removal_price >= 0) with
  | 1, true ->
      removal_price := !removal_price + 2;
      {
        shop with
        player =
          change_gold_player
            (p_remove_card shop.player card)
            (-shop.removal_cost);
        removal_cost = !removal_price;
        card_removal = 0;
      }
  | 1, false ->
      raise
        (NotEnough
           "You checked your pockets and realized that you do not have enough \
            coins.")
  | _, _ -> raise (CardRemoval "The shop is out of card removals.")

let get_player_state (shop : t) = shop.player
