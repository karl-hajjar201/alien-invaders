(* Note: You may introduce new code anywhere in this file. *) 

(* TODO: replace [unit] with a type of your own design. *)
type t = {
  current_r : Adventure.room_id;
  visited_rooms : Adventure.room_id list;
  score : int;
  items: Adventure.item list;
}

let init_state adv ={
  current_r = Adventure.start_room adv;
  visited_rooms = Adventure.start_room adv ::[];
  score = 0;
  items = Adventure.items adv
}

let current_room_id st = st.current_r

let inventory st = 
  List.filter Adventure.in_inventory st.items 



let visited st = st.visited_rooms

type result = Legal of t | Illegal

let get_score st = st.score

let update_score adv st room_id= 
  let current_score = st.score in
  if (List.mem room_id st.visited_rooms) then current_score
  else current_score + Adventure.get_score adv room_id


let go ex adv st =
  let current_room = 
    Adventure.aide (Adventure.room_list adv) (current_room_id st) in
  let exit_room_id = 
    Adventure.helper (Adventure.exit_list current_room) ex in
  if List.mem ex (Adventure.exits adv (current_room_id st)) then 
    Legal {current_r = exit_room_id;
           visited_rooms = [exit_room_id] @ st.visited_rooms;
           score = update_score adv st exit_room_id;
           items = st.items
          }
  else Illegal


let pick st adv item = 
  if Adventure.item_location item = st.current_r then 
    Legal {current_r = st.current_r;
           visited_rooms = st.visited_rooms;
           score = st.score;
           items = (Adventure.pick_up st.items item)
          }
  else Illegal



let drop st adv item = 
  Legal {current_r = st.current_r;
         visited_rooms = st.visited_rooms;
         score = st.score;
         items = (Adventure.drop st.items item st.current_r)
        }
let items_here st = 
  List.filter (Adventure.in_room st.current_r) st.items

let win_condition st adv = 
  if (List.filter (Adventure.in_room (Adventure.target_room adv)) st.items) = st.items then
    true else false