(** The type of chess piece ranks. *)
type rank = Pawn | King | Queen | Knight | Rook | Bishop

(** The type of the teams. *)
type team = White | Black

(** The type of locations of pieces *)
type location = (char * int) option

(** The type of a pice*)
type piece

(** [get_team] returns the team of [piece]. *)
val get_team: piece -> team


(** [get_location] returns the position of piece [piece] *)
val get_location : piece -> location

(** [get_rank] returns the rank of piece [piece] *)
val get_rank : piece -> rank

(** [set_piece] returns piece with [location], [rank],[team],
    [firstmove]. *)
val set_piece: location -> rank -> team -> bool -> piece

(** [blockable_movement] returns true if [p] is a piece with a blockable
    movement scheme. This means it is a Rook, Queen, Bishop. 
    includes Rook, Bishop, and Queen. Otherwise, it will return false. *)
val blockable_movement: piece -> bool

(** [rank_to_string] returns string version of [rank]. *)
val rank_to_string: rank -> string

(** [string_to_rank] returns the rank of [string]. *)
val string_to_rank: string -> rank option
