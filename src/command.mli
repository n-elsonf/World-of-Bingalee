(**Parsing commands from the player*)

type card_name = string
(** Alias for string to represent a card name that is identified in a command*)

type door = int
(** Alias for int to represent picking a door at a crossroads*)

(** Type to represent various commands input by the user across all possible
    situations *)
type command =
  | Play of card_name
  | CheckHand
  | Go of door
  | EndTurn
  | Quit
  | Help
  | TryAgain
  | Buy of card_name
  | Remove of card_name
  | Heal
  | Recharge
  | Leave
  | Info of card_name

exception Empty
(** Raised when an empty command is parsed. *)

exception Malformed
(** Raised when a malformed command is parsed. *)

val parse : string -> command
(** [parse str] parses a player's input into a [command], as follows. The first
    word (i.e., consecutive sequence of non-space characters) of [str] becomes
    the verb. The rest of the words, if any, become the object phrase.

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space
    characters (only ASCII character code 32; not tabs or newlines, etc.).
    Raises: [Empty] if [str] is the empty string or contains only spaces.
    Raises: [Malformed] if the command is malformed. A command is malformed if
    the verb is neither "play" nor "end", or if the verb is "end" and there is a
    non-empty card name, or if the verb is "play" and there is an empty card
    name.*)
