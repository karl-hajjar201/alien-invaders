type team = Black | White
type rank = King | Queen | Bishop | Rook | Knight | Pawn
type location = (char*int) option
(* Option for location because if a piece is deleted, it shouldn't have a location *)

type piece = {
  team: team;
  rank: rank;
  location: location;
  first_move: bool;
}

(** [get_team] returns the team of [piece]. *)

let get_team piece = 
  piece.team

(** [get_rank] returns the rank of piece [piece] *)
let get_rank piece=
  piece.rank

(** [get_location] returns the position of piece [piece] *)
let get_location piece =
  piece.location

(** [set_piece] returns piece with [location], [rank],[team],
    [firstmove]. *)
let set_piece location rank team firstmove= {
  location = location;
  rank = rank;
  team=team;
  first_move= firstmove;
}

(** [blockable_movement] returns true if [p] is a piece with a blockable
    movement scheme. This means it is a Rook, Queen, Bishop. 
    includes Rook, Bishop, and Queen. Otherwise, it will return false. *)
let blockable_movement p = 
  if (get_rank p = Rook || get_rank p = Queen || get_rank p = Bishop)
  then true else false

(** [rank_to_string] returns string version of [rank]. *)
let rank_to_string rank= 
  match rank with
  | Pawn -> "pawn"
  | King -> "king"
  | Queen -> "queen"
  | Knight -> "knight"
  | Rook -> "rook"
  | Bishop -> "bishop"

(** [string_to_rank] returns the rank of [string]. *)
let string_to_rank (string:string) : rank option = 
  match string with
  | "pawn" -> Some Pawn
  | "king" -> Some King
  | "queen" -> Some Queen
  | "knight" -> Some Knight
  | "rook" -> Some Rook
  | "bishop" -> Some Bishop
  | _ -> None

