(**State to keep track of the availability of various healing items and the
   player's stats while in a camp encounter*)

type t
(**abstract type representation of a camp state*)

exception InvalidChoice of string
(**Raised when a player attempts to sleep or recharge multiple times in a row*)

val create_camp : Player.t -> t
(**creates a camp based on the current floor and depth*)

val sleep_health : t -> t
(** [sleep_health p c] is the new camp state with updating player stats after
    they choose to sleep in the camp. Repeatedly sleeping will raise
    InvalidChoice*)

val gatorade_energy : t -> t
(** [gatorade_energy p c] is the new camp state with updating player stats after
    they choose to recharge in the camp. Repeatedly recharging will raise
    InvalidChoice*)

val camp_get_player_state : t -> Player.t
(**gets the player state*)

val exists_energy : t -> bool
(** [camp_energy c] is the energy availability of [c]*)

val exists_hp : t -> bool
(** [camp_health c] is the health availability of [c]*)

val stats : t -> string
(** [stats c] returns the formatted string of the player's stats*)
