open OUnit2
open Player 
open Tile
open Game
open Command

(** The Test Plan:
    A) Parts of the system tested by OUnit and why:
      All functions except display, display related functions, and helper 
      functions are tested by OUnit. This is because OUnit gives rigorous 
      results and is more efficient to implement.
      Specifically, the parts of the system tested by OUnit are 
      1) Player Module: 
      consists of three pieces of logic - riichi, ron, and other player 
      operations:
        i) riichi_tests: check_riichi, riichi
        ii) ron_tests: check_triplet, ron
        iii) player_tests: discard_tile, draw_tile, init_player,ini_comb, 
        ini_info, string_of_yaku (used in printer)
      2) Tile Module: construct, sim_construct, find_tile, ck_adj, ck_eq, 
      ck_seq, ck_tri, chii_legal, all_pos, string_of_tile (used in printer)

    B) Parts of the system tested manually and why:
      All display, display related functions, and helper functions are tested
      manually. This is because we think it is more efficient to see if 
      display/print related functions are functioning correctly by looking at
      them. An example of display function is [dp], which print out the kind and 
      number of a single tile. 
      All game flow related functions are tested manually as well. All commands
      are covered. We believe manually playing the game in terminal is more 
      efficient than writing OUnit tests for them. This is because there are a 
      lot of interaction with users within even one turn, and most state-
      transitioning functions requires user inputs in the middle of them. It is 
      much more difficult to hard code each scenario where a player can type in 
      all kinds of options that lead to different routines and produce 
      different result than to actually play around with the game and examine 
      the output. 
      An example of flow-control related function is [after_chii], which is the 
      state of game after current player at state perfom action chii. In 
      manually testing the game, we tried out all possible commands and options
      a player can choose at this stage, including a normal chii-action, 
      skipping chii action, empty commands, and malformed commands. All are 
      responded robustly by the system.

    C) How OUnit test cases were developed (black box, galss box, ect.):
      Using test-driven development, most test cases are black box tests. 
      However, to make sure the correctness of ron, riichi we also did glass 
      box testing after black box testing on the ron and riichi test suits.

    D) Why testing approach demonstrates then correctness of the system: 
      All functions are tested either with OUnit or manually. The OUnit tests 
      ensure the basic "back-end" pieces of logic are correct. Some of the unit 
      tests check one piece of logic independently in one module, such as 
      [draw_tile] from Player Module. The other OUnit tests test pieces of code 
      that integrate different pieceis of modules to ensure different pieces of 
      the modules are working togeter. For example, ron_output_test uses ron in
      Player module, and ck_eq, ck_seq, ck_tri in Tile module. The manual tests 
      test involves lots of game-play that tests all possible branches of the 
      game flow.
      Black box testing makes sure the functions performs correctly. Glass box 
      testing ensure high test coverage. Test cases include cornor cases, which
      demonstrate the correctness of each piece of logic under different 
      situations. 
*)

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
      | h1 :: (h2 :: t as t') ->
        if n = 100 then acc ^ "..."  (* stop printing long list *)
        else loop (n + 1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

(* Start of tests **********************************)

(* Game tests ******************)
let init_deck = init_state ()
let game1 = Game.make_game init_deck
let game2 = Game.make_game init_deck

(* let print_result1 = display_game game1
   let print_result2 = display_game game2 *)

(* Command Compilation Unit Tests *****************)
(** [command_parse_test] asserts that the parsed command is equal to the 
    ecpected command. *)
let command_parse_test
    (name : string)
    (str : string)
    (expected : command) : test = 
  name >:: (fun _ ->
      assert_equal expected (parse str))

(** [command_parse_test_exn] asserts that the expected exception is raised 
    when parsing an abnormal command. *)
let command_parse_text_exn
    (name : string)
    (str : string)
    (expected_exn) : test =
  name >:: (fun _ ->
      assert_raises expected_exn (fun () -> parse str))

let command_tests = [
  (* typical cases *)
  command_parse_test {|"discard Man 1" -> Discard (Man, 1)|}
    "discard Man 1" (Discard (Man, 1));
  command_parse_test {|"discard man 1" with spaces -> Discard (Man, 1)|}
    "      discard       Man 1" (Discard (Man, 1));
  command_parse_test {|"discard Dragon 1" -> Discard (Man, 1)|}
    "discard Dragon 1" (Discard (Dragon, 1));
  command_parse_test {|"chii 1" -> Chii 1|} "chii 1" (Chii 1);
  command_parse_test {|"Quit" -> Quit|} "quit" Quit;

  (* exn raised *)
  command_parse_text_exn {|"whatever Man 1" -> Malformed|} 
    "whatever Man 1" Malformed;
  command_parse_text_exn {|"Discard Man" -> Malformed|} 
    "Discard Man" Malformed;
  command_parse_text_exn {|"Discard something" -> Malformed|} 
    "Discard something" Malformed;
  command_parse_text_exn {|"" -> Empty|} "" Empty;
]

let _ = print_endline ("finished evaluating command parse test" )

(** Rong test--------------------------------------------- *)
(* tile: id kind num isDiscarded *)
let t1 = Tile.construct 1 Man 1 false
let t2 = Tile.construct 1 Man 2 false
let t3 = Tile.construct 1 Man 3 false
let t4 = Tile.construct 1 Man 4 false
let t5 = Tile.construct 1 Man 5 false
let t6 = Tile.construct 1 Man 6 false
let t7 = Tile.construct 1 Man 7 false
let t8 = Tile.construct 1 Man 8 false
let t9 = Tile.construct 1 Man 9 false

let t11 = Tile.construct 1 Sou 1 false
let t12 = Tile.construct 1 Sou 2 false
let t13 = Tile.construct 1 Sou 3 false
let t14 = Tile.construct 1 Sou 4 false
let t15 = Tile.construct 1 Sou 5 false
let t16 = Tile.construct 1 Sou 6 false
let t17 = Tile.construct 1 Sou 7 false
let t18 = Tile.construct 1 Sou 8 false
let t19 = Tile.construct 1 Sou 9 false

let t21 = Tile.construct 1 Dragon 1 false
let t22 = Tile.construct 1 Dragon 2 false
let t23 = Tile.construct 1 Dragon 3 false

let t31 = Tile.construct 1 Pin 1 false
let t32 = Tile.construct 1 Pin 2 false
let t33 = Tile.construct 1 Pin 3 false
let t34 = Tile.construct 1 Pin 4 false
let t35 = Tile.construct 1 Pin 5 false
let t36 = Tile.construct 1 Pin 6 false

let t41 = Tile.construct 1 Wind 1 false
let t42 = Tile.construct 1 Wind 2 false
let t43 = Tile.construct 1 Wind 3 false

let ron_l1= [t1;t1;t1; t2;t2;t2; t3;t3;t3; t4;t4;t4; t5;t5]
let ron_l2= [t1;t2;t3; t7;t8;t9; t11;t12;t13; t17;t18;t19; t5;t5]
let ron_l3 = [t1;t1;t2;t2; t3;t3;t4;t5; t6;t7;t8;t8; t11; t11]
let ron_l4 = [t1;t1;t1; t2;t2;   t3;t3;t3; t4;t4;t4; t5;t5;t5]
let ron_l5 = [t1;t2; t3;t3;t3;t3; t4;t4;t4;t4; t5;t5;t5;t5]
let ron_l6 = [t21;t21;t21; t22;t22;t22; t15;t16;t16;t16;t17; t23;t23;t23]
let ron_l7 = [t8;t9; t3;t3;t3;t3; t4;t4;t4;t4; t5;t5;t5;t5]
let ron_l8 = [t1;t2;t2;t2;t3;t4]
let ron_l9 = [t2;t2; t5;t6;t7;t7;t8;t9;]
let ron_l10 = [t1;t1; t3;t3; t5;t5; t7;t7; t8;t8; t9;t9; t11;t11]
let ron_l11 = [t2;t2;t2; t3;t3;t3; t4;t5;t6; t7;t7;t7; t18;t18]
let ron_l12 = [t1;t1;t1; t3;t3;t3; t4;t5;t6; t7;t7;t7; t8;t8]
let ron_l13 = [t11;t11;t11; t3;t3;t3; t4;t5;t6; t7;t7;t7; t8;t8]
let ron_l14 = [t11;t11;t11; t3;t3;t3; t4;t5;t6; t21;t21;t21; t8;t8]
let ron_l15 = [t1;t2;t3; t11;t12;t13; t4;t5;t6; t17;t18;t19; t5;t5]
let ron_l16 = [t34;t35;t36; t5;t6; t3;t3;t3; t18;t18;t18; t1;t1; t4]

let n_comb1 = Player.ini_comb ron_l1 true
let n_comb2 = Player.ini_comb ron_l2 true
let n_comb3 = Player.ini_comb ron_l3 true
let n_comb4 = Player.ini_comb ron_l4 true
let n_comb5 = Player.ini_comb ron_l5 true
let n_comb6 = Player.ini_comb ron_l6 true
let n_comb7 = Player.ini_comb ron_l7 true
let n_comb8 = Player.ini_comb ron_l8 true
let n_comb9 = Player.ini_comb ron_l9 true
let n_comb10 = Player.ini_comb ron_l10 false
let n_comb11 = Player.ini_comb ron_l11 false
let n_comb12 = Player.ini_comb ron_l12 false
let n_comb13 = Player.ini_comb ron_l13 false
let n_comb14 = Player.ini_comb ron_l14 false
let n_comb15 = Player.ini_comb ron_l15 false
let n_comb16 = Player.ini_comb ron_l16 true

let n_comb20 = Player.ini_comb
    [t1;t1;t3;t3;t3; t18;t18;t18;t4;t5;t6;t34;t35;t36] true
let n_comb21 = Player.ini_comb 
    [t18;t18;t2;t2;t2;t3;t3;t3;t7;t7;t7;t4;t5;t6] false
let n_comb22 = Player.ini_comb
    [t8;t8;t1;t1;t1; t3;t3;t3;t7;t7;t7;t4;t5;t6] false
let n_comb23 = Player.ini_comb
    [t1;t1;t34;t35;t36;t4;t5;t6;t7;t8;t9; t15;t16;t17] false
let n_comb24 = Player.ini_comb
    [t8;t8; t11;t11;t11; t3;t3;t3;t21;t21;t21;t4;t5;t6]
    false

let ron_test
    (name : string)
    (com : Player.comb)
    (expected_output : bool) : test =
  name >:: (fun _ ->
      assert_equal expected_output ( fst (Player.ron com)) 
        ~printer: string_of_bool)

let pp_bool_yaku tuple = 
  "("^ (string_of_bool (fst tuple)) ^ ", " ^ (string_of_yaku (snd tuple)) ^ ")"

let ron_output_test
    (name : string)
    (com : Player.comb)
    (expected_output : (bool * yaku)) : test =
  name >:: (fun _ ->
      assert_equal expected_output (Player.ron com) ~printer: pp_bool_yaku)

let ron_tests = [
  ron_test "111 222 333 444 55" n_comb1 true;
  ron_test "123 789 123 789 55" n_comb2 true;
  ron_test "1122334567 88 11" n_comb3 false;
  ron_test "111 22 333 444 555 " n_comb4 true;
  ron_test "333 444 555 345 12" n_comb5 true;
  ron_test "rrr ggg 56667 www" n_comb6 true;
  ron_test "333 444 555 345 89" n_comb7 false;
  ron_test "cannot exhaust 12 234" n_comb8 false;
  ron_test "<14 tiles 22 567789" n_comb9 false;
  ron_test "7 pairs" n_comb10 true;
  ron_test "Tanyao only Man222 333 456 777 Sou88" n_comb11 true;
  ron_test "hunyise only Man111 333 456 777 88" n_comb12 true;
  ron_test " not riichi, but has draon triplet" n_comb14 true;
  ron_test " richiied normal" n_comb16 true; 

  ron_output_test "true, riichi" n_comb20 (true, Riichi);
  ron_output_test "true, Tanyao" n_comb21 (true, Tanyao);
  ron_output_test "true, Hunyise" n_comb22 (true, Hunyise);
  ron_output_test "true, draon triplet" n_comb24 (true, Dragontriplet);
  ron_output_test "false, None" n_comb9 (false, None);
]

let _ = print_endline ("finished evaluating ron test" )

(**riichi test *******************)

let check_riichi_test
    (name : string)
    (player : Player.t)
    (expected_output : Tile.t list) : test =
  name >:: (fun _ ->
      assert_equal expected_output (Player.check_riichi player) 
        ~printer: (pp_list Tile.string_of_tile))

let riichi_test
    (name : string)
    (player : Player.t)
    (expected_output : bool) : test =
  name >:: (fun _ ->
      assert_equal expected_output (state_r player) ~printer: string_of_bool)

let player2 = init_player 3 false false [] 
    [t1;t1;t1; t2;t2;t2; t3;t3;t3; t4;t4;t4; t5] []
let player3 = init_player 3 false false [] 
    [t11;t11;t11; t12;t12;t12; t13;t13;t13; t14;t14;t14; t15] []

let player4 = init_player 4 false false [] 
    [t11;t11;t11; t12;t12;t12; t13;t13;t13; t14;t14;t14; t15] []

let _ = riichi player4
let riichi_tests = [
  check_riichi_test "player2 can riichi" player2 [t6;t5;t4;t3;t2];
  check_riichi_test "player3 can riichi" player3 [t16;t15;t14;t13;t12];

  riichi_test "player4 has riichied" player4 true;
  riichi_test "player3 has not riichied" player3 false;
] 

let _ = print_endline ("finished evaluating riichi test" )

(* Tile tests ******************)

let rec display_ll lst = 
  let _ = print_string "[ "  in
  match lst with 
  | [] -> print_string "\n"
  | h :: t -> 
    let _= Player.d_list h in
    print_string " ]";
    display_ll t

let rec lst_to_string r =
  match r with
  | [] -> ""
  | h :: [] -> Tile.string_of_tile h
  | h :: t -> Tile.string_of_tile h ^ ";" ^ (lst_to_string t)

let rec pp_matrix matrix =
  match matrix with
  | [] -> ""
  | h :: t -> "[" ^ (lst_to_string h) ^ "];\n" ^ (pp_matrix t)

let ck_adj_test
    (name : string)
    (t1 : Tile.t)
    (t2 : Tile.t)
    (expected_output : bool) : test =
  name >:: (fun _ ->
      assert_equal expected_output (ck_adj t1 t2)
        ~printer: string_of_bool)

let ck_eq_test
    (name : string)
    (t1 : Tile.t)
    (t2 : Tile.t)
    (expected_output : bool) : test =
  name >:: (fun _ ->
      assert_equal expected_output (ck_eq t1 t2)
        ~printer: string_of_bool)

let ck_seq_test
    (name : string)
    (t1 : Tile.t)
    (t2 : Tile.t)
    (t3 : Tile.t)
    (expected_output : bool) : test =
  name >:: (fun _ ->
      assert_equal expected_output (ck_seq t1 t2 t3)
        ~printer: string_of_bool)

let ck_seq_test
    (name : string)
    (t1 : Tile.t)
    (t2 : Tile.t)
    (t3 : Tile.t)
    (expected_output : bool) : test =
  name >:: (fun _ ->
      assert_equal expected_output (ck_seq t1 t2 t3)
        ~printer: string_of_bool)

let ck_tri_test
    (name : string)
    (t1 : Tile.t)
    (t2 : Tile.t)
    (t3 : Tile.t)
    (expected_output : bool) : test =
  name >:: (fun _ ->
      assert_equal expected_output (ck_tri t1 t2 t3)
        ~printer: string_of_bool)

let all_pos_test
    (name : string)
    (lst : Tile.t list)
    (t : Tile.t)
    (expected_output : Tile.t list list) : test =
  name >:: (fun _ ->
      assert_equal expected_output (all_pos lst t)
        ~printer: pp_matrix)

let pos_l1 = [t1;t2;t4;t5]
let pos_l2 = [t1;t2;t2]
let pos_l3 = [t1;t2;t2;t3;t4;t5]
let pos_l4 = [t11;t12;t1;t2]
let pos_l5 = [t11;t12;t13;t14;t21;t22;t23;t41;t43;t32;t33]

let tile_tests = [
  ck_adj_test "man 1 2" t1 t2 true;
  ck_adj_test "man 1 Sou 2" t1 t12 false;
  ck_adj_test "man 1 Man 3" t1 t3 false;

  ck_eq_test "man1 = man1" t1 t1 true;
  ck_eq_test "dif kind, same num" t1 t11 false;
  ck_eq_test "dif num, same kin" t1 t8 false;

  ck_seq_test "is seq Wind123" t31 t32 t33 true;
  ck_seq_test "not a seq, dif kind" t31 t12 t33 false;
  ck_seq_test "not a seq, wrong num" t11 t15 t13 false;

  ck_tri_test "is triple dragon111" t41 t41 t41 true;
  ck_tri_test "not a triple, dif kind" t41 t11 t41 false;
  ck_tri_test "not a triple, dif num" t41 t42 t41 false;

  all_pos_test "Only Sequence: Man 1245, Man3" 
    pos_l1 t3 [[t2; t3; t4];[t3; t4; t5];[t1; t2; t3]];
  all_pos_test "Only Triplet: Man 22, Man2" 
    pos_l2 t2 [[t2; t2; t2]];
  all_pos_test "Sequence and triplet: Man 1 22 345, Man2" 
    pos_l3 t2 [[t2; t2; t2];[t1; t2; t3];[t2; t3; t4]];
  all_pos_test "Different kinds: Man12 Sou12, Sou3" 
    pos_l4 t13 [[t11; t12; t13]];
  all_pos_test "all kinds, but only Sou 123 valid" 
    pos_l5 t31 [[t31; t32; t33]];
]

let _ = print_endline ("finished evaluating tile test" )

(* Player tests ******************)

let discard_tile_test
    (name : string)
    (player : Player.t)
    (tid : Tile.t option)
    (expected_output : bool) : test =
  name >:: (fun _ ->
      assert_equal expected_output (discard_tile player tid))

let tile1 = Tile.construct 1 Man 1 true
let tile2 = Tile.construct 2 Sou 3 false
let tile3 = Tile.construct 4 Man 2 true
let tile4 = Tile.construct 5 Man 7 false
let tile5 = Tile.construct 6 Wind 2 false
let tile6 = Tile.construct 7 Dragon 2 false
let tile7 = Tile.construct 8 Dragon 1 false

let t_list1 = [tile2; tile4]
let t_list2 = [tile1; tile2; tile1; tile2; tile4; tile5; tile6; tile7]
(* Man127 Sou3 Wind2 Dragon2 Dragon1*)
let dark1 = [tile1; tile2; tile3; tile4; tile5; tile6; tile7]

(* id richii chii light dark discard *)
let player1 = Player.init_player 1 false false [] dark1 t_list1

let chii_legal_test
    (name : string)
    (lst : Tile.t list)
    (t : Tile.t)
    (expected_output : bool) : test =
  name >:: (fun _ ->
      assert_equal expected_output (chii_legal lst t))

let chii_update_handtile_test
    (name : string)
    (player : Player.t)
    (expected_output : Tile.t list list) : test =
  name >:: (fun _ ->
      assert_equal expected_output 
        ([hand_tile_light player] @ [hand_tile_dark player]) 
        ~printer: pp_matrix)

let chii_update_state_c_test
    (name : string)
    (player : Player.t)
    (expected_output : bool) : test =
  name >:: (fun _ ->
      assert_equal expected_output (state_c player) ~printer: string_of_bool)

let draw_tile_test
    (name : string)
    (player : Player.t)
    (expected_output : Tile.t list) : test =
  name >:: (fun _ ->
      assert_equal expected_output (hand_tile_dark player) 
        ~printer: (pp_list Tile.string_of_tile))

let discard_tile_test
    (name : string)
    (player : Player.t)
    (tile_opt : Tile.t option)
    (expected_output : bool) : test =
  name >:: (fun _ ->
      assert_equal expected_output (discard_tile player tile_opt) 
        ~printer: string_of_bool)

let discard_tile_detail_test
    (name : string)
    (player : Player.t)
    (tile_opt : Tile.t option)
    (expected_output : Tile.t list list) : test =
  name >:: (fun _ ->
      if (discard_tile player tile_opt) then 
        assert_equal expected_output ([hand_tile_dark player] @ 
                                      [discard_pile player]) 
          ~printer: pp_matrix else ()
    )

(* id richii chii light dark discard *)
let player11 = Player.init_player 11 false false [] pos_l1 []
let _ = chii_update_handtile 1 t3 player11
let _ = draw_tile player11 t1

let player12 = Player.init_player 12 false false [t1;t1;t1] pos_l2 []
let _ = chii_update_handtile 1 t2 player12
let _ = draw_tile player12 t31

let player13 = Player.init_player 13 false false [] [t1;t2;t3;t4] []

let player14 = Player.init_player 14 false false [] [t1;t1;t1;t2;t3;t4] []

let player_tests = [
  discard_tile_test "discard one existing tile" player1 (Some tile2) true;
  discard_tile_test "discard one not existed tile" player1 None false;

  chii_legal_test "Man 12 45, Man3" pos_l1 t3 true;
  chii_legal_test "Man122, Man2" pos_l2 t2 true;
  chii_legal_test "Man122345, Man2" pos_l3 t2 true;
  chii_legal_test "Man 12345, Sou1, dif kind" pos_l1 t11 false;
  chii_legal_test "Man 12345, Man 8, wrong num" pos_l1 t8 false;
  chii_legal_test "Man 1, Man 2, not enough tiles" [t1] t2 false;
  chii_legal_test "Dragon123" [t21;t22] t23 false;
  chii_legal_test "Dragon111" [t21;t21] t21 true;

  draw_tile_test "draw Man1" player12 [t31;t1];

  chii_update_handtile_test "update chii_handtile" player11 
    [[t2;t3;t4];[t1;t1;t5]];
  chii_update_state_c_test "update state_c" player11 true;

  chii_update_handtile_test "update chii_handtile" player12 
    [[t2;t2;t2;t1;t1;t1];[t31;t1]];
  chii_update_state_c_test "update state_c" player12 true;

  discard_tile_test "discard existing tile Some t2" player13 (Some t2) true;

  discard_tile_test "discard existing tile None" player13 None false;
  discard_tile_detail_test "discard non-existing tile None" player13 None
    [[t1;t2;t3;t4];[]];

  discard_tile_detail_test "player 14: discard existing tile" player14 (Some t1)
    [[t1;t1;t2;t3;t4];[t1]];
]

let _ = print_endline ("finished evaluating player test" )

let suite =
  "test suite for Mahjong" >::: List.flatten [
    command_tests;
    riichi_tests;
    ron_tests;
    tile_tests;
    player_tests;
  ]

let _ = run_test_tt_main suite
