(** Module for managing information about each card, as well as a database of
    cards and their information*)

type t
(** The abstract type of values representing card decks, useful in testing a
    card database, but not for the actual game. *)

type card
(** The abstract type representing a single card.*)

val create_cards : Yojson.Basic.t -> t
(** [create_card j] is the adventure that [j] represents. Requires: [j] is a
    valid JSON card representation. *)

exception UnknownCard of string
(** Raised when an unknown card identifier is encountered. It carries the
    identifier of the unknown card. *)

val description : string -> string
(** [description s] is the description of the card with identifier [s] in the
    cards. Raises [UnknownCard s] if [s] is not a card identifier. *)

val get_card_name : card -> string
(** [get_card_name c] is the name of [c]. *)

val get_dmg : string -> int
(** [get_damage s] is the damage of the card with identifier [s] in the cards.
    Raises [UnknownCard s] if [s] is not a card identifier. *)

val get_energy : string -> int
(** [get_energy s] is the energy of the card with identifier [s] in the cards.
    Raises [UnknownCard s] if [s] is not a card identifier. *)

val get_block : string -> int
(** [get_block s] is the block of the card with identifier [s] in the cards.
    Raises [UnknownCard s] if [s] is not a card identifier. *)

val get_id : string -> string
(** [get_id s] is the id of the card with identifier [s] in the cards. Raises
    [UnknownCard s] if [s] is not a card identifier. *)

val get_tier : string -> int
(** [get_tier s] is the description of the card with identifier [s] in the
    cards. Raises [UnknownCard s] if [s] is not a card identifier. *)

val get_bdmg : string -> int
(**[get_bdmg s] returns the bonus damage value of a card given its name*)

val get_blck : string -> int
(**[get_blck s] returns the bonus block value of a card given its name*)

val get_synergy : string -> string list
(**[get_synergy s] returns a list of cards that have synergy with the given card*)

val get_value : string -> int
(**[get_value s] returns the value of a card given its name*)

val t1_cards : string list
(**[t1_cards] is a list of all tier 1 card names*)

val t2_cards : string list
(**[t2_cards] is a list of all tier 2 card names*)

val t3_cards : string list
(**[t3_cards] is a list of all tier 3 card names*)

val find_card : string -> card
(**[find_card s] returns the card object of a card given its name*)
