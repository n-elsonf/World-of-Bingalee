open Player
open Enemy
open Card
open Command
open Printf
open UsefulFunctions

exception CardNotInHand of string
exception NotEnoughEnergy

type t = {
  player : Player.t;
  enemy : Enemy.t;
  gold : int;
  max_hp : int;
  max_energy : int;
  cur_hp : int;
  cur_energy : int;
  block : int;
  deck : string list;
  used_cards : string list;
  hand : string list;
  active : string list;
  hold : string option;
}

type card = string
(** The abstract type of values representing the game state. *)

(* Old eval_active — KEEP JUST IN CASE NEW DOESN'T WORK *)
(* let rec eval_active (state : t) = match state.active with | [] -> state | h
   :: t -> if t = [] then { state with enemy = change_health_enemy state.enemy
   (get_dmg h) } else eval_active state *)

let change_block (p : t) (nblock : int) = p.block + nblock

let rec eval_active_aux (state : t) =
  match state.active with
  | [] -> state
  | h :: t ->
      let synergy_dmg =
        List.fold_left
          (fun acc c ->
            if List.mem c (get_synergy h) then acc + get_bdmg c + get_bdmg h
            else acc)
          0 t
      in
      let synergy_blk =
        List.fold_left
          (fun acc c ->
            if List.mem c (get_synergy h) then acc + get_blck c + get_blck h
            else acc)
          0 t
      in
      let dmg = get_dmg h + synergy_dmg in
      let blk = get_block h + synergy_blk in
      let enemy' = change_health_enemy state.enemy dmg in
      let block' = change_block state blk in
      let state' = { state with enemy = enemy'; block = block'; active = t } in
      eval_active_aux state'

let eval_active (state : t) =
  { (eval_active_aux state) with active = state.active }

(**plays the card*)
(*let play_card (card_database : Card.t) (card_name : string) (state : t) = if
  List.mem card_name (player_hand state.player) then let new_enemy_state =
  card_name |> get_dmg |> change_health state.enemy in (* print_endline
  (string_of_int (enemy_health new_enemy_state)); *) { state with enemy =
  new_enemy_state } else raise (CardNotInHand card_name)*)

(**returns player hand*)
let checkhand (state : t) = state.hand

let enemy_battle (state : t) = state.enemy

type status =
  | Alive
  | PlayerDead
  | EnemyDead

let game_state (state : t) =
  if state.cur_hp <= 0 then PlayerDead
  else if enemy_health state.enemy <= 0 then EnemyDead
  else Alive

let enemy_attacks (state : t) =
  let damage = enemy_damage state.enemy - state.block in
  { state with cur_hp = (state.cur_hp - if damage < 0 then 0 else damage) }

let hold_card (state : t) (card : string) : t = { state with hold = Some card }

let new_deck (deck : string list) (held_card : string option) : string list =
  match held_card with
  | None -> List.filteri (fun i _ -> i > 4) deck
  | Some card -> List.filteri (fun i _ -> i > 3) deck

let new_hand (deck : string list) (held_card : string option) : string list =
  match held_card with
  | None -> List.filteri (fun i _ -> i <= 4) deck
  | Some card -> card :: List.filteri (fun i _ -> i <= 3) deck

let no_more_cards (p : t) : t =
  let deck2 = p.deck @ shuffle p.used_cards in
  {
    p with
    hand = new_hand deck2 p.hold;
    deck = new_deck deck2 p.hold;
    used_cards = [];
  }

let draw (p : t) : t =
  let p' =
    {
      p with
      used_cards = p.used_cards @ p.active @ p.hand;
      active = [];
      hand = [];
    }
  in
  match p'.hold with
  | None when List.length p'.deck < 5 -> no_more_cards p'
  | Some _ when List.length p'.deck < 4 -> no_more_cards p'
  | _ ->
      {
        p' with
        hand = new_hand p'.deck p'.hold;
        deck = new_deck p'.deck p'.hold;
      }

let get_health_strings (state : t) =
  ( "♡ " ^ string_of_int state.cur_hp ^ "/" ^ string_of_int state.max_hp,
    "♡ "
    ^ string_of_int (enemy_health state.enemy)
    ^ "/"
    ^ string_of_int (enemy_max_health state.enemy) )

let get_card_state (state : t) = (state.active, state.hand)

let activate_card (s : t) (c : card) =
  let rec pluck_card hand removed =
    match hand with
    | [] -> raise (CardNotInHand (removed ^ " not in hand."))
    | h :: t -> if h = removed then t else h :: pluck_card t removed
  in
  match
    let dE = Card.get_energy c in
    if dE <= s.cur_energy then
      {
        s with
        hand = pluck_card s.hand c;
        active = c :: s.active;
        cur_energy = s.cur_energy - dE;
      }
    else raise NotEnoughEnergy
  with
  | x -> x
  | exception UnknownCard card -> raise (CardNotInHand (c ^ " not in hand."))

let gold_on_kill s = { s with gold = s.gold + enemy_gold s.enemy }

let get_player_state (s : t) =
  Player.player_from s.max_hp s.max_energy
    (s.deck @ s.used_cards @ s.hand @ s.active)
    s.gold s.cur_hp (player_stage s.player)

let init_battle (p : Player.t) (enemy_tier : int) =
  {
    player = p;
    enemy = init_enemy enemy_tier;
    gold = player_gold p;
    max_hp = player_health p;
    cur_hp = player_cur_health p;
    max_energy = Player.player_max_energy p;
    cur_energy = Player.player_max_energy p;
    block = 0;
    deck = shuffle (player_cards p);
    used_cards = [];
    hand = [];
    active = [];
    hold = None;
  }
  |> draw

let reset_turn (s : t) = { s with cur_energy = s.max_energy; block = 0 }
let get_cur_energy s = s.cur_energy

let for_player_attack_test enemy active =
  let b = init_battle (Player.create_player ()) 1 in
  { b with enemy; active }
