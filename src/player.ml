open UsefulFunctions
open Card

type t = {
  max_health : int;
  max_energy : int;
  cards : string list;
  gold : int;
  cur_health : int;
  stage : int * int;
}

let create_player () =
  {
    max_health = 50;
    max_energy = 3;
    cards =
      [
        "block";
        "block";
        "block";
        "block";
        "strike";
        "strike";
        "strike";
        "strike";
        "strike";
        "cleave";
      ];
    gold = 10;
    cur_health = 50;
    stage = (1, 0);
  }

let player_from (max_health : int) (max_energy : int) (cards : string list)
    (gold : int) (cur_health : int) (stage : int * int) =
  { max_health; max_energy; cards; gold; cur_health; stage }

let player_health (p : t) : int = p.max_health
let player_cur_health (p : t) : int = p.cur_health
let player_max_energy (p : t) : int = p.max_energy
let player_gold (p : t) : int = p.gold
let player_cards (p : t) : string list = p.cards
let player_gold (p : t) : int = p.gold
let player_stage (p : t) : int * int = p.stage

let add_card (p : t) (card_name : string) : t =
  { p with cards = p.cards @ [ get_id card_name ] }

let p_remove_card (p : t) (card_name : string) : t =
  { p with cards = remove_card p.cards card_name }

let change_player_mhp (p : t) (amount : int) : t =
  { p with max_health = p.max_health + amount }

let change_gold_player (p : t) (gold : int) : t =
  { p with gold = p.gold + gold }

let change_player_menergy (p : t) (amount : int) : t =
  { p with max_energy = p.max_energy + amount }

let change_player_curhp (p : t) (amount : int) : t =
  match p.cur_health + amount <= p.max_health with
  | true -> { p with cur_health = p.cur_health + amount }
  | false -> { p with cur_health = p.max_health }

let crhp_to_max_health p = { p with cur_health = p.max_health }
let is_dead p = p.cur_health <= 0

let create_dead_player () =
  let p = create_player () in
  { p with cur_health = 0 }

let crhp_to_max_health p = { p with cur_health = p.max_health }
let is_dead p = p.cur_health <= 0

let create_dead_player () =
  let p = create_player () in
  { p with cur_health = 0 }
