open Adventure
open State
open Command


let next_state a st exit = 
  match go exit a st with
  |Illegal -> print_endline("That is not a valid exit name"); st
  |Legal next -> next

let next_state_pickup a st item_id = 
  try match pick st a (what_item (items a) item_id) with
    |Illegal -> print_endline("That is not a valid item"); st
    |Legal next -> next
  with _ -> print_endline("Unknown Item"); st



let next_state_drop a st item_id = 
  match drop st a (what_item (items a) item_id) with
  |Illegal -> print_endline("Not in your inventory"); st
  |Legal next -> next

let play_help a st str = 
  match Command.parse str with 
  |Go msg -> next_state a st (String.concat " " msg)
  |Take msg -> next_state_pickup a st (String.concat " " msg)
  |Drop msg -> next_state_drop a st (String.concat " " msg)
  |Inventory -> print_endline(String.concat " " (item_id_ls(inventory st)));st
  |Score -> print_endline(string_of_int(State.get_score st)); st
  |Quit -> print_endline ("Quitting Now. Hope You Enjoyed Playing!"); exit 0
  |exception Command.Empty -> print_endline ("Remember to type something"); st
  |exception Malformed -> print_endline ("This is not a valid command"); st

let rec update1 a st = 
  if (State.win_condition st a) then print_endline("Congrats You Win!") else
    let description = Adventure.description a (current_room_id(st)) in
    print_endline (description);
    print_endline ("Items here are:");
    print_endline (String.concat " " (item_id_ls(items_here st)));
    print_endline "Either type Go somewhere or Quit to exit";
    let command = read_line (()) in
    let next_st = play_help a st command in
    update1 a next_st


(** [play_game f] starts the adventure in file [f]. *)
let play_game f =
  let adventure = try Adventure.from_json(Yojson.Basic.from_file f)
    with _ -> print_endline (String.concat " " [f;"is not a valid Json file"]); 
      exit 0 in
  update1 adventure (init_state adventure)



(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the 3110 Text Adventure Game engine. \n");
  print_endline "Please enter the name of the game file you want to load.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game file_name

(* Execute the game engine. *)
let () = main ()
