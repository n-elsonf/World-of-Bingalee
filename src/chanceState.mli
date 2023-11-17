(**State to manage chance encounters during the game*)

type t
(**The abstract type representing the state where a player is in a chance
   encounter*)

type prompt
(**Type to store pertinent information about a prompt, such as the possible
   outcomes of the player's answer.*)

type change
(**Type to represent effects of an encounter with regards to ]Player.t] attributes*)

val prompt_desc : prompt -> string
(**Returns the text of a particular [ChanceState] prompt*)

val read_decision : t -> bool
(**Gathers the user's input to a prompt and returns a boolean representing a yes
   or no*)

val get_player_state : t -> Player.t
(**Extracts the [Player] object from this State*)

val create_chance_event : Player.t -> t
(**Creates a new chance encounter*)

val apply_prompt : t -> Player.t
(**Carries out the prompt by asking it to the client.*)
