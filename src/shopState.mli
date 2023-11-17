(**State to manage buying and removing of cards from a player's inventory.*)

type t
(**representation of shop*)

exception InvalidPurchase of string
(**Raised when an illegal purchase is attempted, ie a card that doesn't exist.*)

exception CardRemoval of string
(**Raised when the shop can no longer take any more cards off the player's deck*)

exception NotEnough of string
(**Raised when the player doesn't have enough gold to make a purchase*)

val create_shop : Player.t -> t
(**creates a shop based on the current floor and depth*)

val get_cards : t -> string list
(**gets the cards that the shop is selling*)

val get_card_removals : t -> int
(**gets the shop's card removal*)

val get_removal_cost : t -> int
(**gets the shop's card removal cost*)

val buy_card : t -> string -> t
(**the player buys a card from the shop. Raises InvalidPurchase when the player
   buys a card that the shop is not selling. Raises NotEnough when the player
   cannot afford the card*)

val buy_card_removal : t -> string -> t
(**the player buys a card removal from the shop. Raises CardRemoval when the
   player buys a card removal from a shop that does not have any.*)

val get_player_state : t -> Player.t
(**gets the player state*)
