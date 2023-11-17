(**BattleState represents the state of an ongoing battle, storing information on
   the player as well as the enemy and managing attacks and deaths among the two*)

type t
(** The abstract type representing the battle state.*)

exception CardNotInHand of string
(**Raised when the player attempts to perform an action with a card not in their
   hand*)

exception NotEnoughEnergy
(**Raised when the player attempts to play a card they don't have the energy for*)

val init_battle : Player.t -> int -> t
(**[init_battle p flr] returns a new battle state with a fresh enemy for the
   corresponding [flr] value and the data in [p] loaded in*)

val checkhand : t -> string list
(**[checkhand b] returns the list of card names in the players hand*)

val enemy_battle : t -> Enemy.t
(**[enemy_battle b] returns the enemy in the battle represented by [b]*)

type status =
  | Alive
  | PlayerDead
  | EnemyDead
      (**The type representing the state of the battle; whether either enemy or
         player is dead or if both are alive*)

val game_state : t -> status
(**[game_state b] returns Alive if both player and enemy are alive, PlayerDead
   if the player is dead, and EnemyDead if the player is alive and the enemy is
   dead. In the unlikely case both are dead, PlayerDead is returned.*)

val enemy_attacks : t -> t
(**[enemy_attacks b] returns a new battle state taking into account damage done
   by one enemy turn*)

val get_card_state : t -> string list * string list
(**[get_card_state b] returns a string list of the current active cards as well
   as the hand of the player in battle*)

val activate_card : t -> string -> t
(**[activate_card b c] returns a new state after attempting to move a card [c]
   in hand to the active pile. Raises CardNotInHand if [c] isn't in the hand and
   raises NotEnoughEnergy if [c] has a greater energy cost than the player can
   handle*)

val get_player_state : t -> Player.t
(**[get_player_state b] returns the Player.t object fully updated with the
   information that took place in the battle currently stored in [b]*)

val eval_active : t -> t
(** [eval_active b] returns a new battle state representing the point in time
    after the player "steps" their turn once, playing their active hand*)

val draw : t -> t
(** [draw b] reutrns a new battle state after tossing out the old hand and
    activated cards and replacing the hand with new cards from the deck*)

val reset_turn : t -> t
(**[reset_turn b] returns a new battle state after the player and enemy have
   both taken their turn, resetting attributes like the player's block and
   energy*)

val gold_on_kill : t -> t
(**[gold_on_kill b] returns a new state updated to reflect the gold gained by
   killing the enemy currently in [b]*)

val get_health_strings : t -> string * string
(**[get_health_strings b] returns a tuple of formatted strings representing the
   current player and enemy's healths in the battle represented by [b]*)

val get_cur_energy : t -> int
(**[get_cur_energy b] returns the player's current energy at the moment
   represented by [b]*)

val for_player_attack_test : Enemy.t -> string list -> t
(**[for_player_attack_test e active] returns a new battle where the player is
   already loaded with [active] in their active slot and facing [e]. This is
   helpful for testing specific active evaluations*)
