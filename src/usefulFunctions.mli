(**UsefulFunctions provides various miscellaneous functions that share usage
   across multiple modules.*)

val shuffle : string list -> string list
(**Shuffles a string list and returns the result*)

val remove_card : string list -> string -> string list
(**[remove_card cl c] Removes the first occurrence of [c] from the left in [cl]
   and returns the result*)

val join_slist : string list -> string -> string
(**[join_slist sl sep] converts [sl] into one long string separated by [sep]*)
