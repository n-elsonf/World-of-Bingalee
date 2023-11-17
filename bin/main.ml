open Game

exception Restart
exception End

let print_battle_instructions () =
  ANSITerminal.(
    print_string [ green; Underlined ] "\n=== Instructions for Battle ===\n");
  print_endline
    "\n   play <card> : plays the card with name <card> in your hand";
  print_endline "\n   info <card> : gives the <card> description";
  print_endline
    "\n   end : ends your turn after you have played your desired cards";
  print_endline "\n   quit : quits the game";
  print_endline "\n   again: allows you to have a second chance and try to win ";
  print_endline "\n   info <card> : prints a description of the card"

let print_door_instructions () =
  ANSITerminal.(
    print_string [ green; Underlined ] "\n=== Instructions for Hallway ===\n");
  print_endline "\n   go <door> : leads you to the encounter behind <door> "

let print_shop_instructions () =
  ANSITerminal.(
    print_string [ green; Underlined ] "\n=== Instructions for Shop ===\n");
  print_endline
    "\n   checkhand : displays cards in your active slot and your hand";
  print_endline "\n   buy <card>: buy a <card> available at the shop";
  print_endline "\n   remove <card>: removes a <card> from your hand";
  print_endline "\n   leave : removes you from the shop"

let print_camp_instructions () =
  ANSITerminal.print_string
    [ ANSITerminal.green; ANSITerminal.Underlined ]
    "\n=== Instructions for Camp ===\n";
  print_endline "\n   heal : heals you by 8%";
  print_endline "\n   recharge : increases yous energy by 1";
  print_endline "\n   leave : removes you from the camp"

let enemy_hit (state : BattleState.t) =
  let enemy = BattleState.enemy_battle state in
  let face = Enemy.enemy_face enemy in
  match Enemy.enemy_name enemy with
  | "slime" ->
      ANSITerminal.print_string [ ANSITerminal.green; ANSITerminal.Bold ] face;
      ANSITerminal.print_string [ ANSITerminal.Bold ]
        "\n\n A SLIME BABY STARTS CRAWLING TOWARDS YOU...\n"
  | "bird" ->
      ANSITerminal.print_string [ ANSITerminal.blue; ANSITerminal.Bold ] face;
      ANSITerminal.print_string [ ANSITerminal.Bold ]
        "\n\n A VULTURE STARTS FLYING IN YOUR DIRECTION\n"
  | "vampire" ->
      ANSITerminal.print_string [ ANSITerminal.red; ANSITerminal.Bold ] face;
      ANSITerminal.print_string [ ANSITerminal.Bold ]
        "\n\n A VAMPIRE SEES THE BLOOD ON YOU AND SPEEDS TO YOU\n"
  | "robot" ->
      ANSITerminal.print_string [ ANSITerminal.blue; ANSITerminal.Bold ] face;
      ANSITerminal.print_string [ ANSITerminal.Bold ]
        "\n\n A ROBOT ROLLS TOWARDS YOU \n"
  | "mary" ->
      ANSITerminal.print_string [ ANSITerminal.white; ANSITerminal.Bold ] face;
      ANSITerminal.print_string [ ANSITerminal.Bold ]
        "\n\n BLOODY MARY HAS APPEARED AND STARTS TO HAUNT YOU \n"
  | "zombie" ->
      ANSITerminal.print_string [ ANSITerminal.yellow; ANSITerminal.Bold ] face;
      ANSITerminal.print_string [ ANSITerminal.Bold ]
        "\n\n ZOMBIE SLOWLY STARTS WALKING TO YOU\n"
  | "ghost" ->
      ANSITerminal.print_string [ ANSITerminal.white; ANSITerminal.Bold ] face;
      ANSITerminal.print_string [ ANSITerminal.Bold ]
        "\n\n A GHOST FLOATS TOWARDS YOU\n"
  | "clown" ->
      ANSITerminal.print_string [ ANSITerminal.cyan; ANSITerminal.Bold ] face;
      ANSITerminal.print_string [ ANSITerminal.Bold ]
        "\n\n\
        \ PENNYWISE WALKS TOWARDS YOU AND STARTS TURNING INTO YOUR BIGGEST FEAR \n"
  | _ -> failwith "Invalid enemy"

let check_hand s =
  let active, hand = BattleState.get_card_state s in
  ANSITerminal.print_string [ ANSITerminal.Bold; ANSITerminal.green ] "Active: ";
  print_endline (UsefulFunctions.join_slist active ", ");
  ANSITerminal.print_string [ ANSITerminal.Bold; ANSITerminal.magenta ] "Hand: ";
  print_endline (UsefulFunctions.join_slist hand ", ")

let print_battlefield (s : BattleState.t) =
  print_endline "\n\n";
  print_string ("  𖨆\t\t" ^ Enemy.enemy_face (BattleState.enemy_battle s));
  print_endline "";
  let ph, eh = BattleState.get_health_strings s in
  print_endline (ph ^ "\t\t" ^ eh);
  print_endline ("Energy: ⚡" ^ string_of_int (BattleState.get_cur_energy s));
  print_endline "";
  check_hand s

let read_input () =
  ANSITerminal.print_string [ ANSITerminal.green; ANSITerminal.Bold ] "> ";
  let open Command in
  () |> read_line |> parse

let play_card c s =
  let open BattleState in
  match activate_card s c with
  | exception CardNotInHand msg ->
      ANSITerminal.print_string [ ANSITerminal.red; ANSITerminal.Bold ] msg;
      s
  | exception NotEnoughEnergy ->
      ANSITerminal.print_string
        [ ANSITerminal.Bold; ANSITerminal.red ]
        "Not enough energy.\n";
      s
  | s' -> s'

let check_hand_from_player p =
  ANSITerminal.(print_string [ Underlined; blue ] "Cards:\n");
  print_endline
    ("   - " ^ UsefulFunctions.join_slist (Player.player_cards p) "\n   - ")

let end_turn s =
  let open BattleState in
  let s' = s |> eval_active |> draw in
  if game_state s' = EnemyDead then (
    ANSITerminal.(print_string [ green; Bold ] "You won the battle!\n");
    (true, false, gold_on_kill s'))
  else
    let s'' = enemy_attacks s' in
    ANSITerminal.(print_string [ blue; Bold ] "You played your turn\n");
    enemy_hit s'';
    if game_state s'' = PlayerDead then (false, true, s'')
    else (false, false, reset_turn s'')

let info c =
  let open Card in
  match description c with
  | s ->
      ANSITerminal.print_string
        [ ANSITerminal.blue; ANSITerminal.Underlined ]
        (c ^ "\n");
      print_string ("   " ^ s)
  | exception UnknownCard s -> print_endline s

let battle (p : Player.t) (flr : int) =
  let rec battle_loop s =
    print_battlefield s;
    match read_input () with
    | Play c ->
        let s' = play_card c s in
        battle_loop s'
    | EndTurn -> (
        match end_turn s with
        | true, _, s' -> s'
        | _, true, s' -> raise Restart
        | _, _, s' -> battle_loop s')
    | Quit -> raise End
    | Info c ->
        info c;
        battle_loop s
    | Help ->
        print_battle_instructions ();
        battle_loop s
    | _ | (exception Command.Malformed) | (exception Command.Empty) ->
        ANSITerminal.(print_string [ red; Bold ] "Invalid command.");
        battle_loop s
  in
  flr |> BattleState.init_battle p |> battle_loop
  |> BattleState.get_player_state

let display_doors doors =
  print_endline "";
  ANSITerminal.(print_string [ Bold; cyan ] "You are at a crossroads...\n\n");
  ignore
    (List.fold_left
       (fun i s ->
         print_endline ("  -> Door " ^ string_of_int i ^ ": " ^ s ^ "\n");
         i + 1)
       1 doors);
  ANSITerminal.(print_string [ Bold; cyan ] "Select a door...\n")

let door () =
  let rec door_loop () =
    match read_input () with
    | Go i -> i
    | Quit -> raise End
    | Help ->
        print_door_instructions ();
        door_loop ()
    | _ | (exception Command.Malformed) | (exception Command.Empty) ->
        ANSITerminal.(print_string [ red; Bold ] "Invalid command.\n");
        door_loop ()
  in
  let door_choices = Encounter.generate_encounters () in
  display_doors door_choices;
  List.nth door_choices (door_loop () - 1)

let your_gold s =
  s |> ShopState.get_player_state |> Player.player_gold |> string_of_int

let print_shop s =
  ANSITerminal.print_string
    [ ANSITerminal.magenta; ANSITerminal.Bold ]
    "\n\
     You're in a small, decrepid shop. The merchant\n\
     behind the counter peers at you warily.\n";
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\tYou can't help but notice the glitter in his\n\
     \teyes as he glances at your coin purse...\n\
     \tOr was it your imagination?\n\n";
  ANSITerminal.print_string
    [ ANSITerminal.magenta; ANSITerminal.Underlined ]
    "Cards For Sale\n";
  let card_list = ShopState.get_cards s in
  if List.length card_list = 0 then
    ANSITerminal.(print_string [ Bold; red ] "SOLD OUT\n\n")
  else
    print_endline
      ("   - " ^ UsefulFunctions.join_slist card_list "\n   - " ^ "\n");
  ANSITerminal.print_string
    [ ANSITerminal.magenta; ANSITerminal.Underlined ]
    "Card Removal Cost:";
  print_endline (" " ^ string_of_int (ShopState.get_removal_cost s) ^ "\n");
  ANSITerminal.(print_string [ yellow; Bold ] "Your gold: ");
  print_endline (your_gold s)

let shop (p : Player.t) =
  let open ShopState in
  let rec shop_loop s =
    print_shop s;
    match read_input () with
    (* | Help -> shop_help *)
    | Buy c -> (
        match buy_card s c with
        | x -> shop_loop x
        | exception InvalidPurchase m ->
            ANSITerminal.(print_string [ Bold; red ] m);
            shop_loop s
        | exception NotEnough m ->
            ANSITerminal.(print_string [ Bold; red ] m);
            shop_loop s)
    | Remove c -> (
        match buy_card_removal s c with
        | x -> shop_loop x
        | exception CardRemoval m ->
            ANSITerminal.(print_string [ Bold; red ] m);
            shop_loop s
        | exception NotEnough m ->
            ANSITerminal.(print_string [ Bold; red ] m);
            shop_loop s)
    | Leave -> get_player_state s
    | CheckHand ->
        check_hand_from_player (get_player_state s);
        shop_loop s
    | Quit -> raise End
    | Help ->
        print_shop_instructions ();
        shop_loop s
    | _ | (exception Command.Malformed) | (exception Command.Empty) ->
        ANSITerminal.(print_string [ red; Bold ] "Invalid command.\n");
        shop_loop s
  in
  shop_loop (create_shop p)

let print_camp c =
  let open CampState in
  print_endline "\n";
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    "A cool breeze blows through your hair... It's peaceful here.";
  print_endline "\n";
  if exists_energy c then print_endline "\tA Gatorade bottle lies at your feet."
  else print_endline "\tAn empty Gatorade bottle lies at your feet.";
  if exists_hp c then
    print_endline "\tNext to the bottle, you see a cozy looking blanket."
  else
    print_endline
      "\tNext to the bottle, you see a cozy blanket, but you're not tired.";
  print_endline "";
  print_endline (stats c)

let camp (p : Player.t) =
  let open CampState in
  let rec camp_loop c =
    print_camp c;
    match read_input () with
    | Heal -> (
        match c |> sleep_health with
        | c' ->
            ANSITerminal.print_string [ ANSITerminal.green ]
              "You lay down to take a nap. Health increased!";
            print_endline (string_of_int (Player.player_cur_health p));
            camp_loop c'
        | exception CampState.InvalidChoice s ->
            ANSITerminal.print_string [ ANSITerminal.red ] s;
            camp_loop c)
    | Recharge -> (
        match c |> gatorade_energy with
        | c' ->
            ANSITerminal.print_string [ ANSITerminal.yellow ]
              "You chug the bottle of gatorade. Electrolytes course through \
               your body! Max energy increased!";
            camp_loop c'
        | exception CampState.InvalidChoice s ->
            ANSITerminal.print_string [ ANSITerminal.red ] s;
            camp_loop c)
    | Leave ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          "You leave the camp and all it's peacefullness. \n\
           You miss the feeling of ease already...\n\
           but the grind must go on...\n\n";
        c
    | Quit -> raise End
    | Help ->
        print_camp_instructions ();
        camp_loop c
    | _ | (exception Command.Malformed) | (exception Command.Empty) ->
        ANSITerminal.(print_string [ red; Bold ] "Invalid command.\n");
        camp_loop c
  in
  p |> create_camp |> camp_loop |> camp_get_player_state

let chance p =
  let open ChanceState in
  let p' = p |> create_chance_event |> apply_prompt in
  if Player.is_dead p' then raise Restart else p'

let encounter p flr dep =
  match door () with
  | "Battle" -> battle p flr
  | "Shop" -> shop p
  | "Camp" -> camp p
  | "Chance" -> chance p
  | _ -> failwith "not possible"

let rec restart () =
  match read_input () with
  | TryAgain -> true
  | Quit -> false
  | _ | (exception Command.Malformed) | (exception Command.Empty) ->
      ANSITerminal.(print_string [ red; Bold ] "Invalid command.\n");
      restart ()

let rec floor p flr =
  match
    ANSITerminal.print_string [ ANSITerminal.green ]
      "\n\n\
      \    You step into a new realm... eager to explore.\n\
      \    It's not long before you're ambushed!";
    let p1 = battle p flr in
    let p2 = encounter p1 flr 2 in
    let p3 = encounter p2 flr 3 in
    let p4 = encounter p3 flr 4 in
    ANSITerminal.print_string
      [ ANSITerminal.red; ANSITerminal.Bold ]
      "\nA boss approaches...";
    let p5 = battle p4 (flr + 3) in
    p5
  with
  | exception Restart ->
      ANSITerminal.print_string
        [ ANSITerminal.Bold; ANSITerminal.red ]
        "You died.\n\n";
      print_endline
        {|type "again" to try once more or "quit" to end the adventure|};
      if restart () then floor p flr else raise End
  | exception End -> raise End
  | p' -> p'

let adventure_begin () =
  match
    (*** START ***)
    let p0 = Player.create_player () in
    let p1 = floor p0 1 in
    let p2 = floor p1 2 in
    ignore (floor p2 3);
    ANSITerminal.(print_string [ green; Bold ] "You Win!\n")
  with
  | exception End -> ANSITerminal.(print_string [ red; Bold ] "fArEwELl...\n")
  | _ -> ()

let main () =
  print_endline "";
  ANSITerminal.resize 130 130;
  ANSITerminal.print_string
    [ ANSITerminal.Bold; ANSITerminal.cyan ]
    "Welcome to the World of Bingalee\n";
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    "The <help> command will give you instuctions on how to play and what \
     commands are at your disposal";
  adventure_begin ()

let () = main ()
