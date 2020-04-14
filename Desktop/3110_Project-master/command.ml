(* Note: You may introduce new code anywhere in this file. *) 

type object_phrase = string list
type location = string list (**Change this to a better/dif data type *)

type command = 
  |Pawn of location
  |Knight of location
  |Rook of location
  |King of location
  |Queen of location
  |Bishop of location
  |Forfit
  | Go of object_phrase
  |Score
  |Inventory
  |Take of object_phrase
  |Drop of object_phrase
  | Quit

exception Empty
exception Malformed

(** [isnt_space s] returns true iff [s] is not an empty string *)
let isnt_space s =
  if s = "" then false else true

let parse str =
  let str_list = List.filter isnt_space 
      (String.split_on_char ' ' (String.trim(String.lowercase_ascii str))) in
  match str_list with
  |[]-> raise Empty
  |["forfit"] -> Forfit
  |["score"] -> Score
  |["inventory"] -> Inventory
  |["quit"] -> Quit
  |h::t -> if h = "take" then if List.length t < 1 then raise Malformed else Take t 
    else if h = "drop" then if List.length t < 1 then raise Malformed else Drop t
    else if h = "go" then if List.length t < 1 then raise Malformed else Go t 
    (** Real Commands*)
    else if h = "knight" then if List.length t < 1 then raise Malformed else Knight t
    else if h = "king" then if List.length t < 1 then raise Malformed else King t
    else if h = "pawn" then if List.length t < 1 then raise Malformed else Pawn t
    else if h = "queen" then if List.length t < 1 then raise Malformed else Queen t
    else if h = "bishop" then if List.length t < 1 then raise Malformed else Bishop t
    else if h = "rook" then if List.length t < 1 then raise Malformed else Rook t


    else raise Malformed

