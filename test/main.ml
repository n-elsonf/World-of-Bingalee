(***** TEST PLAN: The automated test suite using OUnit was helpful in testing
  single interactions between states. Since our game is fundamentally built off
  the exchange of various states across different "environments," such as a
  shop, camp, battle, etc, we used OUnit to verify that states were correctly
  updating with the right information as well as being passed along correctly to
  other states. Another example where OUnit was helpful was in verifying that
  string inputs to a command parser still resulted in the right command
  Variants, despite having extra white space and whatnot. OUnit was also helpful
  for verifying slightly more complex operations, such as single turns in a
  battle. The synergy feature we provide between cards is not very
  straightforward in damage calculation, so OUnit allowed us to have a
  controlled environment to observe the single outcome. What we had to test
  manually via [make play] was the actual adventure from start to finish, as
  well as progressing between environments like shops to battles, etc. This
  testing took place manually since the actual engine runs as the bridge between
  the operations we test with OUnit.

  Specific modules that were tested by OUnit include: BattleState, CampState,
  Card, Command, Encounter, Enemy, Player, ShopState, and UsefulFunctions. We
  used a combination of black box and glass box testing. For example, the
  testing of the synergy between two cards involved a few black box unit tests,
  as all we know is the outcome to an enemy's health and not the gory details
  about how synergy is identified in the code. For many other tests, we used
  glass box testing to narrow in on small operations, such as parsing Commands,
  which we know strips away extraneous white space. We also test our enemy tier
  system, a system that is completely hidden to the game player but is
  nevertheless important to test for our implementation purposes. Overall, our
  combination of blackbox and glassbox testing was effective in elucidating
  issues and gaining confidence in our system.

  Once again, our system essentially functions based on passing around states.
  For example, the player state is a recurring source of information that is
  continuously updated/overwritten over the course of the game. The player state
  is passed into other states, such as the shop and battle states, and its
  information is extracted and used for calculations regarding those
  environments. As this is the core feature of our game, the way we test our
  modules—and our state modules in particular by verifying the correct
  transferral of information—should naturally give us confidence in the entire
  system working. Because we can verify all the small operations work in fine
  detail, we are confident the more complex operations that go into the final
  game are also correct on this foundation.*)

open Game
open Yojson.Basic.Util
open OUnit2
open Card
open Command
open Player
open Enemy
open BattleState
open CampState
open OUnit2
open ShopState

let data_dir_prefix = "data" ^ Filename.dir_sep
let card_json = Yojson.Basic.from_file (data_dir_prefix ^ "card.json")
let enemy_json = Yojson.Basic.from_file (data_dir_prefix ^ "enemy.json")

(* command test functions *)
let parse_test (name : string) str (expected_output : command) : test =
  name >:: fun _ -> assert_equal expected_output (parse str)

let exn_parse_test (name : string) str expected_output : test =
  name >:: fun _ -> assert_raises expected_output (fun _ -> parse str)

(* player test functions *)
let player_health_test (name : string) (player : Player.t) expected_output :
    test =
  name >:: fun _ -> assert_equal expected_output (player_health player)

let player_cur_health_test (name : string) (player : Player.t) expected_output :
    test =
  name >:: fun _ -> assert_equal expected_output (player_cur_health player)

let player_max_energy_test (name : string) (player : Player.t) expected_output :
    test =
  name >:: fun _ -> assert_equal expected_output (player_max_energy player)

let player_cards_test (name : string) (player : Player.t) expected_output : test
    =
  name >:: fun _ -> assert_equal expected_output (player_cards player)

let player_stage_test (name : string) (player : Player.t) expected_output : test
    =
  name >:: fun _ -> assert_equal expected_output (player_stage player)

let add_card_test (name : string) (player : Player.t) (card : string)
    expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (add_card player card |> player_cards)

let p_remove_card_test (name : string) (player : Player.t) (card : string)
    expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (p_remove_card player card |> player_cards)

let change_player_mhp_test (name : string) (player : Player.t) (amount : int)
    expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (change_player_mhp player amount |> player_health)

let change_gold_player_test (name : string) (player : Player.t) (amount : int)
    expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (change_gold_player player amount |> player_gold)

let change_player_menergy_test (name : string) (player : Player.t)
    (amount : int) expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output
    (change_player_menergy player amount |> player_max_energy)

let change_player_curhp_test (name : string) (player : Player.t) (amount : int)
    expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output
    (change_player_curhp player amount |> player_cur_health)

let enemy_tier_test name tier expected_output : test =
  name >:: fun _ -> assert_equal expected_output (enemy_names (enemy_tier tier))

let enemy_health_test name enemy expected_output : test =
  name >:: fun _ -> assert_equal expected_output (enemy_health enemy)

let player_attack_test (name : string) (enemy : string) (cards : string list)
    expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Enemy.enemy_health
       (BattleState.enemy_battle
          (eval_active (for_player_attack_test (Enemy.enemy_from enemy) cards))))
    ~printer:string_of_int

let get_card_removals_test (name : string) (shop : ShopState.t) expected_output
    : test =
  name >:: fun _ -> assert_equal expected_output (get_card_removals shop)

let get_removal_cost_test (name : string) (shop : ShopState.t) expected_output :
    test =
  name >:: fun _ -> assert_equal expected_output (get_removal_cost shop)

let buy_first shop = buy_card shop (List.nth (get_cards shop) 0)

let buy_remove_first shop =
  buy_card_removal shop (List.nth (shop |> get_player_state |> player_cards) 0)

let enemy_face_test name enemy expected_output : test =
  name >:: fun _ -> assert_equal expected_output (enemy_face enemy)

let enemy_max_health_test name enemy expected_output : test =
  name >:: fun _ -> assert_equal expected_output (enemy_max_health enemy)

let player_block_test (name : string) (enemy : string) (cards : string list)
    expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Player.player_cur_health
       (BattleState.get_player_state
          (BattleState.enemy_attacks
             (eval_active
                (for_player_attack_test (Enemy.enemy_from enemy) cards)))))
    ~printer:string_of_int

let get_card_removals_test (name : string) (shop : ShopState.t) expected_output
    : test =
  name >:: fun _ -> assert_equal expected_output (get_card_removals shop)

let get_removal_cost_test (name : string) (shop : ShopState.t) expected_output :
    test =
  name >:: fun _ -> assert_equal expected_output (get_removal_cost shop)

let buy_first shop = buy_card shop (List.nth (get_cards shop) 0)

let buy_remove_first shop =
  buy_card_removal shop (List.nth (shop |> get_player_state |> player_cards) 0)

let enemy_face_test name enemy expected_output : test =
  name >:: fun _ -> assert_equal expected_output (enemy_face enemy)

let enemy_max_health_test name enemy expected_output : test =
  name >:: fun _ -> assert_equal expected_output (enemy_max_health enemy)

let enemy_gold_test name enemy expected_output : test =
  name >:: fun _ -> assert_equal expected_output (enemy_gold enemy)

let enemy_damage_test name enemy expected_output : test =
  name >:: fun _ -> assert_equal expected_output (enemy_damage enemy)

let enemy_name_test name enemy expected_output : test =
  name >:: fun _ -> assert_equal expected_output (enemy_name enemy)

let change_health_enemy_test name enemy damage expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (change_health_enemy enemy damage |> enemy_health)

(* camp test functions *)
let exists_energy_test name camp expected_output : test =
  name >:: fun _ -> assert_equal expected_output (exists_energy camp)

let exists_hp_test name camp expected_output : test =
  name >:: fun _ -> assert_equal expected_output (exists_hp camp)

let sleep_health_bool_test name camp expected_output : test =
  name >:: fun _ -> assert_equal expected_output (sleep_health camp |> exists_hp)

let sleep_health_int_test name camp expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output
    (sleep_health camp |> camp_get_player_state |> player_cur_health)

let gatorade_energy_bool_test name camp expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (gatorade_energy camp |> exists_energy)

let gatorade_energy_int_test name camp expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output
    (gatorade_energy camp |> camp_get_player_state |> player_max_energy)

let stats_camp_test name camp expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (stats camp) ~printer:String.escaped

let card_description_test name card expected_output : test =
  name >:: fun _ -> assert_equal expected_output (description card)

let exn_cdescription_test (name : string) card expected_output : test =
  name >:: fun _ -> assert_raises expected_output (fun _ -> description card)

let card_dmg_test name card expected_output : test =
  name >:: fun _ -> assert_equal expected_output (get_dmg card)

let exn_cdmg_test (name : string) card expected_output : test =
  name >:: fun _ -> assert_raises expected_output (fun _ -> get_dmg card)

let card_energy_test name card expected_output : test =
  name >:: fun _ -> assert_equal expected_output (get_energy card)

let exn_cenergy_test (name : string) card expected_output : test =
  name >:: fun _ -> assert_raises expected_output (fun _ -> get_energy card)

let card_block_test name card expected_output : test =
  name >:: fun _ -> assert_equal expected_output (get_block card)

let exn_cblock_test (name : string) card expected_output : test =
  name >:: fun _ -> assert_raises expected_output (fun _ -> get_block card)

let card_id_test name card expected_output : test =
  name >:: fun _ -> assert_equal expected_output (get_id card)

let exn_cid_test (name : string) card expected_output : test =
  name >:: fun _ -> assert_raises expected_output (fun _ -> get_id card)

let card_tier_test name card expected_output : test =
  name >:: fun _ -> assert_equal expected_output (get_tier card)

let exn_ctier_test name card expected_output : test =
  name >:: fun _ -> assert_raises expected_output (fun _ -> get_tier card)

let cbonusdmg_test name card expected_output : test =
  name >:: fun _ -> assert_equal expected_output (get_bdmg card)

let exn_cbonusdmg_test name card expected_output : test =
  name >:: fun _ -> assert_raises expected_output (fun _ -> get_bdmg card)

let cbonusblock_test name card expected_output : test =
  name >:: fun _ -> assert_equal expected_output (get_blck card)

let exn_cbonusblock_test name card expected_output : test =
  name >:: fun _ -> assert_raises expected_output (fun _ -> get_blck card)

let cvalue_test name card (expected_output : int) : test =
  name >:: fun _ -> assert_equal expected_output (get_value card)

let exn_cvalue_test name card expected_output : test =
  name >:: fun _ -> assert_raises expected_output (fun _ -> get_value card)

let csynergy_test name card expected_output : test =
  name >:: fun _ -> assert_equal expected_output (get_synergy card)

let exn_csynergy_test name card expected_output : test =
  name >:: fun _ -> assert_raises expected_output (fun _ -> get_synergy card)

let loot_enemy (name : string) (enemy : string) (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Player.player_gold
       BattleState.(
         get_player_state
           (gold_on_kill (for_player_attack_test (Enemy.enemy_from enemy) []))))

let card_tests =
  let card = "clothesline" in
  let card2 = "strike" in
  let card3 = "block" in
  let card4 = "barricade" in
  let card5 = "sentinel" in
  let fake = "fake" in
  [
    card_description_test
      "clothesline has description {|Deals 4 DMG. If you play GERMAN SUPLEX, you\n\
      \       deal 12 DMG instead.|}" card
      "Deals 4 DMG. If you play GERMAN SUPLEX, you deal 12 DMG instead.";
    exn_cdescription_test "fake card doesnt have a decription" fake
      (UnknownCard "Not a valid card.");
    card_description_test "strike has description {|Deals 2 DMG|}." card2
      "Deals 2 DMG";
    card_description_test "block has description {|Blocks 4 DMG|}." card3
      "Blocks 4 DMG";
    card_dmg_test "clothesline deals 4 dmg" card 4;
    card_dmg_test "strike has 2 DMG" card2 2;
    card_dmg_test "block has 0 DMG" card3 0;
    exn_cdmg_test "fake card doesnt have damage" fake
      (UnknownCard "Not a valid card.");
    card_energy_test "clothesline use 1 energy" card 1;
    card_energy_test "strike has 1 energy" card 1;
    card_energy_test "block has 2 energy" card3 2;
    exn_cenergy_test "fake card doesnt have energy" fake
      (UnknownCard "Not a valid card.");
    card_block_test "clothesline blocks 0 damage" card 0;
    card_block_test "block blocks 4 DMG" card3 4;
    card_block_test "strike blocks 0 DMG" card2 0;
    exn_cblock_test "fake card doesnt have block" fake
      (UnknownCard "Not a valid card.");
    card_id_test "clothesline has id clothesline" card "clothesline";
    card_id_test "id of strike is strike" card2 "strike";
    card_id_test "id of block is block" card3 "block";
    exn_cid_test "fake card doesnt have an id" fake
      (UnknownCard "Not a valid card.");
    card_tier_test "clothesline is a tier 1 card" card 1;
    card_tier_test "tier of strike is 1" card2 1;
    card_tier_test "tier of block is 1" card3 1;
    card_tier_test "tier of barricade is 3" card4 3;
    card_tier_test "tier of sentinel is 2" card5 2;
    exn_ctier_test "fake case doesnt have tier" fake
      (UnknownCard "Not a valid card.");
    cbonusdmg_test "clothesline has 2 bonus dmg" card 2;
    cbonusdmg_test "strike has 0 bonus dmg" card2 0;
    cbonusdmg_test "block has 0 bonus dmg" card3 0;
    exn_cbonusdmg_test "fake does not have bonus dmg" fake
      (UnknownCard "Not a valid card.");
    cbonusblock_test "clothesline has 0 bonus block" card 0;
    cbonusblock_test "strike has 0 bonus block" card2 0;
    cbonusblock_test "block has 0 bonus block" card3 0;
    cbonusblock_test "barricade has 0 bonus block" card4 0;
    cbonusblock_test "sentinel has 2 bonus block" card5 2;
    exn_cbonusblock_test "fake has no bonus block" fake
      (UnknownCard "Not a valid card.");
    csynergy_test "clothesline synergizes with german suplex" card
      [ "german suplex" ];
    csynergy_test "barricade synergizes with sentinel" card4 [ "sentinel" ];
    csynergy_test "strike has no synergy with any other card" card2 [];
    csynergy_test "block hasno synergy" card3 [];
    exn_csynergy_test "fake DNE" fake (UnknownCard "Not a valid card.");
    cvalue_test "strike costs 1 gold" card2 1;
    cvalue_test "barricade costs 10 gold" card4 10;
    exn_cvalue_test "fake DNE" fake (UnknownCard "Not a valid card.");
  ]

let command_tests =
  [
    parse_test "extra spaces with play" "play  clash " (Play "clash");
    parse_test "testing if checkhand works" "checkhand" CheckHand;
    parse_test "go command with valid input" "go 1 " (Go 1);
    parse_test "endturn\n   command" "end" EndTurn;
    parse_test "quit command" "quit" Quit;
    parse_test "help command" "help" Help;
    parse_test "tryagain command" "again" TryAgain;
    parse_test "buy command" "buy cleave   " (Buy "cleave");
    parse_test "remove command" "remove cleave " (Remove "cleave");
    parse_test "heal command" "heal" Heal;
    parse_test "recharge\n   command" "recharge" Recharge;
    parse_test "leave command" "leave" Leave;
    parse_test "info command" "info strike" (Info "strike");
    exn_parse_test "go command with invalid input" "go omega attack " Malformed;
    exn_parse_test
      "malfromed exception when end\n   has a non-empty string after end"
      "end hkeje" Malformed;
    exn_parse_test
      "malfromed exception when string has a empty string after play" "play "
      Malformed;
    exn_parse_test "malfromed exception when first word is not play or end"
      "omega attack" Malformed;
    exn_parse_test "empty exception when string\n   contains empty spaces" " "
      Empty;
    exn_parse_test "empty exception when string is the empty string" "" Empty;
  ]

let player_tests =
  let player = create_player () in
  [
    player_health_test "Init Player Max Health" player 50;
    player_cur_health_test "Init Player Curr Health" player 50;
    player_max_energy_test "Init Player Max Energy" player 3;
    player_cards_test "Init Player Cards" player
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
    player_stage_test "Init Player Stage" player (1, 0);
    add_card_test "Add parry to Player Cards" player "parry"
      (player_cards player @ [ "parry" ]);
    ( "Add Nonexistent Card to Player Cards" >:: fun _ ->
      assert_raises (UnknownCard "Not a valid card.") (fun _ ->
          add_card player "NONEXISTENT") );
    p_remove_card_test "Remove parry from Player Cards"
      (add_card player "parry") "parry" (player_cards player);
    p_remove_card_test "Removing an nonexistent card does nothing" player
      "NONEXISTENT" (player_cards player);
    change_player_mhp_test "Increase Max HP by 5" player 5 55;
    change_player_mhp_test "Decrease Max HP by 5" player (-5) 45;
    change_gold_player_test "Give Player 10 Gold" player 10 20;
    change_gold_player_test "Remove Player 10 Gold" player (-10) 0;
    change_gold_player_test "Give Player 10 Gold" player 10 20;
    change_gold_player_test "Remove Player 10 Gold" player (-10) 0;
    change_player_menergy_test "Increase Max Energy by 1" player 1 4;
    change_player_menergy_test "Decrease Max Energy by 1" player (-1) 2;
    change_player_curhp_test "Healing for 10 HP when already full HP" player 10
      50;
    change_player_curhp_test "Player takes damage and loses 10 HP" player (-10)
      40;
    change_player_curhp_test "Healing for 10 HP when player is at 40 HP"
      (change_player_curhp player (-10))
      10 (player_health player);
  ]

let enemy_tests =
  let enemy = init_enemy 2 in
  [
    enemy_health_test "health of generated enemy(robot)" enemy 25;
    enemy_gold_test "gold of generated enemy(robot)" enemy 2;
    enemy_damage_test "damage of generated enemy(robot)" enemy 5;
    enemy_name_test "name of generated enemy is robot " enemy "robot";
    enemy_face_test "face of generated enemy is d[o_0]b " enemy "d[o_0]b";
    enemy_max_health_test "maximum health of generated enemy(robot)" enemy 25;
    change_health_enemy_test "decreasing health to 5" enemy 20 5;
    change_health_enemy_test "increasing health to 30" enemy (-5) 30;
    enemy_tier_test "tier 1 enemies" 1 [ "slime"; "bird" ];
    enemy_tier_test "tier 2 enemies" 2 [ "robot" ];
    enemy_tier_test "tier 3 enemies" 3 [ "zombie"; "ghost" ];
    enemy_tier_test "tier 4 enemies" 4 [ "vampire" ];
    enemy_tier_test "tier 5 enemies" 5 [ "mary" ];
    enemy_tier_test "tier 6 enemies" 6 [ "clown" ];
  ]

let camp_tests =
  let player = create_player () in
  let camp = create_camp player in
  [
    exists_energy_test "health available after arrival to camp" camp true;
    exists_energy_test "energy available after arrival to camp" camp true;
    sleep_health_bool_test "health unavailable after heal" camp false;
    gatorade_energy_bool_test "gatorade unavailable after recharge" camp false;
    sleep_health_int_test "health stays at 50 after heal" camp 50;
    gatorade_energy_int_test "gatorade increases max energy to 4 after recharge"
      camp 4;
  ]

let shop_tests =
  let player = create_player ()
  and player2 = change_gold_player (create_player ()) (-100) in
  let shop = create_shop player and shop2 = create_shop player2 in
  [
    get_card_removals_test "Init Shop Number of Card Removals" shop 1;
    get_removal_cost_test "Init Shop Card Removal Cost" shop 3;
    ( "Player buys the first card in shop: Gets the card" >:: fun _ ->
      assert_equal
        (buy_card shop (List.nth (get_cards shop) 0)
        |> get_player_state |> player_cards)
        (List.nth (get_cards shop) 0 |> add_card player |> player_cards) );
    ( "Player buys the first card in shop: Loses money" >:: fun _ ->
      assert_equal
        (buy_first shop |> get_player_state |> player_gold)
        (List.nth (get_cards shop) 0
        |> get_value |> ( ~- ) |> change_gold_player player |> player_gold) );
    ( "Player buys the first card in shop: Shop doesn't have that card anymore"
    >:: fun _ ->
      assert_equal (buy_first shop |> get_cards) (get_cards shop |> List.tl) );
    ( "Player buys an nonexistent card" >:: fun _ ->
      assert_raises (UnknownCard "Not a valid card.") (fun _ ->
          buy_card shop "NONEXISTENT") );
    ( "Player tries to buy a card that the shop does not have" >:: fun _ ->
      assert_raises (InvalidPurchase "The shopkeeper isn't selling that card.")
        (fun _ -> buy_card shop "german suplex") );
    ( "Player cannot afford a card" >:: fun _ ->
      assert_raises
        (NotEnough
           "You checked your pockets and realized that you do not have enough \
            coins.") (fun _ -> buy_first shop2) );
    ( "Player buys a card removal: Player removes his first card" >:: fun _ ->
      assert_equal
        (buy_remove_first shop |> get_player_state |> player_cards)
        (player |> player_cards |> List.tl) );
    ( "Player buys a card removal: Player removes his first card" >:: fun _ ->
      assert_equal
        (buy_remove_first shop |> get_player_state |> player_cards)
        (player |> player_cards |> List.tl) );
    ( "Player buys a card removal: Player loses money" >:: fun _ ->
      assert_equal
        (buy_remove_first shop |> get_player_state |> player_gold)
        (get_removal_cost shop |> ( ~- )
        |> change_gold_player (get_player_state shop)
        |> player_gold) );
    ( "Player cannot buy multiple card removals in the same shop" >:: fun _ ->
      assert_raises (CardRemoval "The shop is out of card removals.") (fun _ ->
          buy_card_removal
            (buy_card_removal (buy_remove_first shop)
               (List.nth (shop |> get_player_state |> player_cards) 0))) );
  ]

let battle_tests =
  [
    player_attack_test "being a pacifist with no active does not fail" "slime"
      [] 10;
    player_attack_test "striking a slime once" "slime" [ "strike" ] 8;
    player_attack_test "striking a slime twice" "slime" [ "strike"; "strike" ] 6;
    player_attack_test "strike a slime thrice" "slime"
      [ "strike"; "strike"; "strike" ]
      4;
    player_attack_test "block does nothing to slime" "slime" [ "block" ] 10;
    player_attack_test "block and strike does same as strike" "slime"
      [ "block"; "strike" ] 8;
    player_block_test "block reduces slime damage to 0" "slime" [ "block" ] 50;
    player_block_test "block and strike reduces slime damate" "slime"
      [ "block"; "strike" ] 50;
    player_block_test "block reduces bird damage to 0" "bird" [ "block" ] 50;
    player_block_test "block reduces vampire damage to 1" "vampire" [ "block" ]
      49;
    player_block_test "double block stacks against vampire" "vampire"
      [ "block"; "block" ] 50;
    player_attack_test "parrying against mary deals damage" "mary" [ "parry" ]
      26;
    player_block_test "parrying against mary blocks damage" "mary" [ "parry" ]
      48;
    player_attack_test "double parry against mary stacks" "mary"
      [ "parry"; "parry" ] 22;
    player_block_test "double parry against mary stacks blocks" "mary"
      [ "parry"; "parry" ] 50;
    loot_enemy "looting slime nets 10 + 1 = 11" "slime" 11;
    loot_enemy "looting ghost nets 10 + 4 = 14" "ghost" 14;
  ]

let synergy_tests =
  [
    player_attack_test "clothesline + german suplex" "bird"
      [ "clothesline"; "german suplex" ]
      8;
    player_attack_test "reversed still works" "bird"
      [ "german suplex"; "clothesline" ]
      8;
    player_attack_test "clothes + german + german double bonus" "clown"
      [ "clothesline"; "german suplex"; "german suplex" ]
      35;
    player_block_test "sentinel + barricade takes no damage" "clown"
      [ "sentinel"; "barricade" ]
      50;
    player_block_test "reversed version also works" "clown"
      [ "barricade"; "sentinel" ]
      50;
  ]

let suite =
  "test suite for Final Project"
  >::: List.flatten
         [
           command_tests;
           player_tests;
           enemy_tests;
           camp_tests;
           card_tests;
           battle_tests;
           synergy_tests;
           shop_tests;
         ]

let _ = run_test_tt_main suite
