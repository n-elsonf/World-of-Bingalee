open Yojson.Basic.Util

exception UnknownCard of string
exception SynergyError of string

type card = {
  id : string;
  description : string;
  energy : int;
  damage : int;
  block : int;
  value : int;
  tier : int;
  bonusdmg : int;
  bonusblk : int;
  synergy : string list;
}

type t = { cards : card list }

let card_of_json j =
  {
    id = j |> member "id" |> to_string;
    description = j |> member "description" |> to_string;
    energy = j |> member "energy" |> to_int;
    damage = j |> member "damage" |> to_int;
    block = j |> member "block" |> to_int;
    value = j |> member "value" |> to_int;
    tier = j |> member "tier" |> to_int;
    bonusdmg = j |> member "bonusdmg" |> to_int;
    bonusblk = j |> member "bonusblk" |> to_int;
    synergy = j |> member "synergy" |> to_list |> List.map to_string;
  }

let all_cards_of_json j =
  j |> member "cards" |> to_list |> List.map card_of_json

let rec card_description (c : string) (lst : card list) =
  match lst with
  | [] -> raise (UnknownCard "Not a valid card.")
  | h :: t -> if h.id = c then h.description else card_description c t

let rec card_value (c : string) (lst : card list) =
  match lst with
  | [] -> raise (UnknownCard "Not a valid card.")
  | h :: t -> if h.id = c then h.value else card_value c t

let rec card_tier (c : string) (lst : card list) =
  match lst with
  | [] -> raise (UnknownCard "Not a valid card.")
  | h :: t -> if h.id = c then h.tier else card_tier c t

let rec card_dmg (c : string) (lst : card list) =
  match lst with
  | [] -> raise (UnknownCard "Not a valid card.")
  | h :: t -> if h.id = c then h.damage else card_dmg c t

let rec card_energy (c : string) (lst : card list) =
  match lst with
  | [] -> raise (UnknownCard "Not a valid card.")
  | h :: t -> if h.id = c then h.energy else card_energy c t

let rec card_block (c : string) (lst : card list) =
  match lst with
  | [] -> raise (UnknownCard "Not a valid card.")
  | h :: t -> if h.id = c then h.block else card_block c t

let rec card_id (c : string) (lst : card list) =
  match lst with
  | [] -> raise (UnknownCard "Not a valid card.")
  | h :: t -> if h.id = c then h.id else card_id c t

let rec card_tier (c : string) (lst : card list) =
  match lst with
  | [] -> raise (UnknownCard "Not a valid card.")
  | h :: t -> if h.id = c then h.tier else card_tier c t

let rec card_bdmg (c : string) (lst : card list) =
  match lst with
  | [] -> raise (UnknownCard "Not a valid card.")
  | h :: t -> if h.id = c then h.bonusdmg else card_bdmg c t

let rec card_bblk (c : string) (lst : card list) =
  match lst with
  | [] -> raise (UnknownCard "Not a valid card.")
  | h :: t -> if h.id = c then h.bonusblk else card_bblk c t

let rec card_synergy (c : string) (lst : card list) =
  match lst with
  | [] -> raise (UnknownCard "Not a valid card.")
  | h :: t -> if h.id = c then h.synergy else card_synergy c t

let rec card_value (c : string) (lst : card list) =
  match lst with
  | [] -> raise (UnknownCard "Not a valid card.")
  | h :: t -> if h.id = c then h.value else card_value c t

let create_cards j = { cards = all_cards_of_json j }
let data_dir_prefix = "data" ^ Filename.dir_sep
let card_json = Yojson.Basic.from_file (data_dir_prefix ^ "card.json")
let set = create_cards card_json
let get_card_name (card : card) = card.id
let description (card : string) = set.cards |> card_description card
let get_dmg (card : string) = set.cards |> card_dmg card
let get_energy (card : string) = set.cards |> card_energy card
let get_block (card : string) = set.cards |> card_block card
let get_id (card : string) = card_id card set.cards
let get_tier (card : string) = set.cards |> card_tier card
let get_bdmg (card : string) = set.cards |> card_bdmg card
let get_blck (card : string) = set.cards |> card_bblk card
let get_synergy (card : string) = set.cards |> card_synergy card
let get_value (card : string) = set.cards |> card_value card
let is_t1 card = if card.tier = 1 then true else false
let is_t2 card = if card.tier = 2 then true else false
let is_t3 card = if card.tier = 3 then true else false

let find_card (id : string) =
  List.find (fun c -> c.id = id) (all_cards_of_json card_json)

let t1_cards =
  all_cards_of_json card_json |> List.filter is_t1 |> List.map get_card_name

let t2_cards =
  all_cards_of_json card_json |> List.filter is_t2 |> List.map get_card_name

let t3_cards =
  all_cards_of_json card_json |> List.filter is_t3 |> List.map get_card_name
