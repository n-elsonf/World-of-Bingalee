open Random
open UsefulFunctions

type t =
  | Battle
  | Shop
  | Camp
  | Chance

let to_string = function
  | Battle -> "Battle"
  | Shop -> "Shop"
  | Camp -> "Camp"
  | Chance -> "Chance"

let get_random () = List.nth [ Battle; Shop; Camp; Chance ] (Random.int 4)

let generate_encounters () =
  let creating_list = get_random () :: get_random () :: [ Battle ] in
  List.map to_string creating_list |> shuffle
