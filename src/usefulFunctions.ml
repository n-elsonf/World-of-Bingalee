(**[swap] swaps the elemenet at pos1 with the element at pos2*)
let swap (arr : string array) (pos1 : int) (pos2 : int) =
  let temp = arr.(pos1) in
  arr.(pos1) <- arr.(pos2);
  arr.(pos2) <- temp

let shuffle (lst : string list) : string list =
  let arr = Array.of_list lst in
  for i = Array.length arr - 1 downto 1 do
    swap arr i (Random.int (Array.length arr))
  done;
  Array.to_list arr

let rec remove_card (lst : string list) (card_name : string) : string list =
  match lst with
  | [] -> []
  | h :: t -> if h = card_name then t else h :: remove_card t card_name

let join_slist sl sep =
  match sl with
  | [] -> ""
  | [ h ] -> h
  | h :: t -> List.fold_left (fun acc s -> acc ^ sep ^ s) h t
