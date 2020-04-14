open OUnit2
open Adventure
open Command
open State

(********************************************************************
   Here are some helper functions for your testing of set-like lists. 
 ********************************************************************)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1::(h2::t as t') ->
        if n=100 then acc ^ "..."  (* stop printing long list *)
        else loop (n+1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

(* These tests demonstrate how to use [cmp_set_like_lists] and 
   [pp_list] to get helpful output from OUnit. *)
let cmp_demo = 
  [
    "order is irrelevant" >:: (fun _ -> 
        assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
          ["foo"; "bar"] ["bar"; "foo"]);
    (* Uncomment this test to see what happens when a test case fails.
       "duplicates not allowed" >:: (fun _ -> 
        assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
          ["foo"; "foo"] ["foo"]);
    *)
  ]

(********************************************************************
   End helper functions.
 ********************************************************************)

(* You are welcome to add strings containing JSON here, and use them as the
   basis for unit tests.  Or you can add .json files in this directory and
   use them, too.  Any .json files in this directory will be included
   by [make zip] as part of your CMS submission. *)
let adv1 = from_json(Yojson.Basic.from_file "ho_plaza.json")
let adv2 = from_json(Yojson.Basic.from_file "lonely_room.json")

let adventure_tests =
  [
    (* TODO: add tests for the Adventure module here *)

    (** Testing the start rooms of the 2 json files given*)
    "start of ad1" >:: (fun _ -> assert_equal "ho plaza" (start_room adv1));
    "start of ad2" >:: (fun _ -> assert_equal "the room" (start_room adv2));

    (*Testing room_ids*)
    "room_ids of ad1" >:: 
    (fun _ -> assert_equal ["nirvana"; "tower"; "health"; "ho plaza"] 
        (room_ids adv1));

    "room_ids of ad1" >:: 
    (fun _ -> assert_equal ["the room";] (room_ids adv2));

    (*Testing description*)
    "Desrp of adv2 the room" >:: 
    (fun _ -> assert_equal "A very lonely room." (description adv2 "the room"));

    (*Testing exits*)
    "Exits of adv1 tower" >:: 
    (fun _ -> assert_equal ["higher"; "Ho Plaza"; "back"; "down"] 
        (exits adv1 "tower"));

    (*Testing next_room*)
    "next room of adv1 ho plaza exiting south west" >:: 
    (fun _ -> assert_equal "health" (next_room adv1 "ho plaza" "south west"));

    (*Testing next_rooms*)
    "next rooms of adv1 ho plaza" >:: 
    (fun _ -> assert_equal ["tower"; "health"] (next_rooms adv1 "ho plaza"));

  ]

let command_tests =
  [
    "go test case" >:: (fun _ -> assert_equal (Go ["ho";"plaza"]:command)
                           (parse " Go     hO    plAza  "));
    "quit test case" >:: (fun _ -> assert_equal (Quit:command)
                             (parse " quit  "));
    "Empty test case" >:: (fun _ -> assert_raises (Empty)
                              (fun _ -> parse "  " ));
    "Malformed test case" >:: (fun _ -> assert_raises (Malformed)
                                  (fun _ -> parse " nah cheif you messed up" ));
  ]


let legal_test (name:string) (ex:exit_name) (adv:Adventure.t) (st:State.t)
    (expect_room:room_id) (expected_visited: room_id list) =
  name >:: (fun _ -> let result = go ex adv st in match result with
    |Illegal -> failwith("legal test is wrong")
    |Legal t -> assert_equal expect_room (current_room_id t); 
      assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string) 
        expected_visited (visited t)
    )

let illegal_test name ex adv st =
  name >:: (fun _ -> assert_equal Illegal (go ex adv st))

let state = init_state adv1

let state_tests =
  [
    (* TODO: add tests for the State module here *)
    legal_test "legal test" "southwest" adv1 state "health" ["ho plaza"; "health"];
    illegal_test "name" "home" adv1 state;
  ]

let suite =
  "test suite for A2"  >::: List.flatten [
    adventure_tests;
    command_tests;
    state_tests;
  ]

let _ = run_test_tt_main suite
