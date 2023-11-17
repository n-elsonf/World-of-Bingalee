open Card
open Player
open Random
open Yojson.Basic.Util

exception InvalidPrompt of string

type change = {
  maxhp_delta : int;
  health_delta : int;
  energy_delta : int;
  gold_delta : int;
}

type prompt = {
  choice : bool;
  changes : change list;
  description : string;
}

type t = {
  player : Player.t;
  prompt : prompt;
}

let change_of_json (j : Yojson.Basic.t) =
  {
    maxhp_delta = j |> member "maxhp_delta" |> to_int;
    health_delta = j |> member "health_delta" |> to_int;
    energy_delta = j |> member "energy_delta" |> to_int;
    gold_delta = j |> member "gold_delta" |> to_int;
  }

let prompt_of_json (j : Yojson.Basic.t) =
  {
    choice = j |> member "choice" |> to_string |> bool_of_string;
    changes = j |> member "changes" |> to_list |> List.map change_of_json;
    description = j |> member "description" |> to_string;
  }

let all_prompts_of_json j =
  j |> member "prompts" |> to_list |> List.map prompt_of_json

let data_dir_prefix = "data" ^ Filename.dir_sep
let prompts_json = Yojson.Basic.from_file (data_dir_prefix ^ "prompts.json")
let prompts_data = all_prompts_of_json prompts_json

let generate_random_prompt () =
  List.nth prompts_data (Random.int (List.length prompts_data))

let prompt_desc (p : prompt) = p.description
let create_chance_event p = { player = p; prompt = generate_random_prompt () }

let print_player_stats state =
  let player = state.player in
  ANSITerminal.(
    print_string [ magenta ]
      ("CURRENT STATS : "
      ^ string_of_int (player_health player)
      ^ " MAXIMUM HP | "
      ^ string_of_int (player_cur_health player)
      ^ " CURRENT HP | "
      ^ string_of_int (player_max_energy player)
      ^ " MAX ENERGY | "
      ^ string_of_int (player_gold player)
      ^ " GOLD\n"))

let apply_changes (state : t) (change : change) =
  ANSITerminal.(
    print_string [ cyan ]
      ("CHANGES : "
      ^ string_of_int change.maxhp_delta
      ^ " MAXIMUM HP | "
      ^ string_of_int change.health_delta
      ^ " HP | "
      ^ string_of_int change.energy_delta
      ^ " MAX ENERGY | "
      ^ string_of_int change.gold_delta
      ^ " GOLD\n"));

  let updated_state =
    change_player_mhp
      (change_player_menergy
         (change_gold_player
            (change_player_curhp state.player change.health_delta)
            change.gold_delta)
         change.energy_delta)
      change.maxhp_delta
  in

  print_player_stats { state with player = updated_state };
  updated_state

let read_decision (state : t) =
  let rec loop () =
    ANSITerminal.(print_string [ yellow ] "Enter your choice (y/n): ");
    match String.trim (read_line ()) with
    | "y" | "Y" -> true
    | "n" | "N" -> false
    | _ ->
        ANSITerminal.(
          print_string [ red ] "Invalid choice. Please enter 'y' or 'n'.\n");
        loop ()
  in
  loop ()

let apply_prompt (state : t) =
  ANSITerminal.(print_string [ green ] (state.prompt.description ^ "\n"));
  if state.prompt.choice then
    match read_decision state with
    | true -> apply_changes state (List.nth state.prompt.changes 0)
    | false -> apply_changes state (List.nth state.prompt.changes 1)
  else apply_changes state (List.nth state.prompt.changes 0)

let get_player_state (state : t) = state.player
