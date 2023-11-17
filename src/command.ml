open UsefulFunctions

type card_name = string
type door = int

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

let rec split_on_space (s : string list) acc =
  match s with
  | [] -> List.rev acc
  | h :: t ->
      if h = "" then split_on_space t acc else split_on_space t (h :: acc)

let fst_list = function
  | [] -> failwith "impossible to reach"
  | h :: t -> h

let object_list (lst : string list) =
  match lst with
  | [] -> raise Empty
  | h :: t ->
      if h = "play" && t != [] then Play (String.trim (join_slist t " "))
      else if h = "checkhand" && t = [] then CheckHand
      else if h = "go" && (t = [ "1" ] || t = [ "2" ] || t = [ "3" ]) then
        Go (t |> fst_list |> int_of_string)
      else if h = "end" && t = [] then EndTurn
      else if h = "quit" && t = [] then Quit
      else if h = "help" && t = [] then Help
      else if h = "again" && t = [] then TryAgain
      else if h = "buy" && t != [] then Buy (String.trim (join_slist t " "))
      else if h = "remove" && t != [] then Remove (join_slist t " ")
      else if h = "heal" && t = [] then Heal
      else if h = "recharge" && t = [] then Recharge
      else if h = "leave" && t = [] then Leave
      else if h = "info" && t != [] then Info (String.trim (join_slist t " "))
      else raise Malformed

let parse str =
  if str = "" then raise Empty
  else split_on_space (String.split_on_char ' ' str) [] |> object_list
