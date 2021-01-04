(** AF: a record 
    {wall_tiles = [Tile1; Tile2; ... ; Tilen ]; 
    player = [Player1; Player2; Player3; Player4];
    current_player_id = n;} represents a state of the 
    game with 
    - wall tiles {Tile1, Tile2, ... , Tilen} and 
    - players {Player1, ... ,Player4}.
    - current player [Player n]

    RI: [wall_tiles] must be valid tiles initialized by a tile constructor.
        [players] must contain 4 players. [current_player_id] is 1, 2, 3, or 4. 
*)
type game_state = {
  (* [wall_tiles] is the deck of unassigned tiles from which players draw. 
      [wall_tiles] contains all the tiles before the game starts. *)
  wall_tiles : Tile.t list;
  (* a list of players *)
  players : Player.t list;
  current_player_id : Player.id;
  in_game : bool
}

type t = game_state 

(** [is_in_game state] is whether the game is still going on. Updated to false
    whenever a player has won. *)
let is_in_game state = state.in_game

(** [mps_of_id id] generates a Min, Pin, or Sou tile according to its id by 
    the following rules:
    Man, Pin, Sou:
    - represented by id = a * 9 + x, where id <= 108
    - if a = 0, 3, 6, 9 => Man
    - if a = 1, 4, 7, 10 => Pin
    - if a = 2, 5, 8, 11 => Sou
    - x + 1 = number on tile
    - e.g. id = 3 = 0 * 0 + 3 => Man 3 *)
let mps_of_id id =
  let a = (id - 1) / 9 in
  let x = 1 + (id mod 9) in
  if a = 0 || a = 3 || a = 6 || a = 9 then (Tile.Man, x)
  else if a = 1 || a = 4 || a = 7 || a = 10 then (Tile.Pin, x)
  else (Tile.Sou, x)

(** [wind_of_id id] generates a Wind tile according to its id by the following
    rules: 
    Wind:
    - represented by id = 81 + b * 4 + y, where id > 108
    - b = 0, 1, 2, 3
    - y = 1, 2, 3, 4
    - y = 1 => Wind 1 => East Wind
    - y = 2 => Wind 2 => South Wind
    - y = 3 => Wind 3 => West Wind
    - y = 4 => Wind 4 => North Wind 
    - e.g. id = 82 = 0 * 4 + 1 + 81 => Wind 1 => East Wind *)
let wind_of_id id =
  let y = 1 + (id - 81) mod 4 in (Tile.Wind, y)

(** [dragon_of_id id] generates a Dragon tile according to its id by the 
    following rules: 
    Dragon:
    - id = 124 + c * 3 + z, where id > 124
    - c = 0, 1, 2
    - z = 1, 2, 3
    - Dragon z *)
let dragon_of_id id = 
  let z = 1 + (id - 124) mod 3 in (Tile.Dragon, z)

(* [tile_of_id id] is a tile calculated from a given [id]. The relation 
   between a tile and a id is described above. *)
let tile_of_id id = 
  let kind, n = begin
    if id <= 108 then mps_of_id id
    else if id <= 124 then wind_of_id id
    else dragon_of_id id
  end in
  Tile.construct id kind n false

(* [init_tiles id acc] initializes a list of tiles with id = 1 - [id]. *)
let rec init_tiles id acc = 
  if id = 0 then acc
  else init_tiles (id - 1) (tile_of_id id :: acc)

(** [init_state ()] is a game state where 
    - [wall_tiles] contains all tiles needed in the game
    - [players] is not populated with players yet.
    - [current_player_id] starts from 1. *)
let init_state () =
  {
    wall_tiles = init_tiles 136 [];
    players = []; (* no players yet *)
    current_player_id = 1;
    in_game = true;
  }

(** [extract not_picked n lst] extracts the [n]th element from [lst] and get 
    the list with remaining elements after extraction. This cannot be done with
    [List.nth] or [List.nth_opt] since the remaining list is needed to ensure
    that no dupicated elements are extracted when extracting multiple 
    elements, and to update the game's wall tiles with whats left after 
    extraction. *)
let rec extract not_picked n lst = 
  match lst with
  | [] -> raise Not_found
  | h :: t -> begin
      if n = 0 then (h, not_picked @ t)
      else extract (h :: not_picked) (n - 1) t
    end 

(** [extract_n lst n acc op] extracts n elements and put then in a new list and
    get the list with remaining elements after extraction. It either extract 
    randomly or sequentially depending on [op].
    [extract_n] = (extracted, left) *)
let rec extract_n lst n acc op = 
  if n = 0 then (acc, lst)
  else 
    let picked, not_picked = op lst in
    extract_n not_picked (n - 1) (picked :: acc) op

(** [extract_rand lst] extracts one random element from [lst]. Evaluates to a 
    tuple (picked, not_picked). *)
let extract_rand lst = 
  let n = lst |> List.length |> Random.int in
  extract [] n lst

(** [shuffle_list lst] is the shuffled [lst]. *)
let shuffle_list lst = 
  Random.self_init ();
  let shuffled, left = extract_n lst (List.length lst) [] extract_rand in
  shuffled

(** [extract_seq lst] extracts the first element in [lst]. It is the op passed 
    into [extract_n] for sequentially extracting the first n elements. This 
    cannot be done conviniently with [List.hd] since whats left after 
    extraction is needed to update the game's wall tiles. *)
let extract_seq lst =
  extract [] 0 lst

(** [extract_first_n lst n] extracts first [n] elementes for [lst] and 
    construct a new list from them. Evaluates to (extracted, left) *)
let extract_first_n lst n = 
  extract_n lst n [] extract_seq

(** [make_game state] is a game state where 
    - [wall_tiles] is shuffled and first 4 * 13 tiles are given to each player
    - [players] has 4 players and each player with 13 hand tiles. *)
let make_game state = 
  let shuffled_tiles = shuffle_list state.wall_tiles in
  let rec assign n_of_p left acc = 
    if n_of_p = 0 then (acc, left)
    else begin
      let hand, left = extract_first_n left 13 in
      let hand' = Tile.sort hand in
      let player = Player.init_player n_of_p false false [] hand' [] in
      assign (n_of_p - 1) left (player :: acc) 
    end in
  let players, walls = assign 4 shuffled_tiles [] in
  { state with players = players; wall_tiles = walls}

(* pretty-print helpers ************************************)

(** [string_of_combo combo] convert a combination of tiles represented by a 
    list [combo] to a string. *)
let  string_of_combo combo =
  List.fold_left 
    (fun acc tile -> acc ^ "  " ^ Tile.string_of_tile tile) "" combo

(** [string_of_all_combos combos] convert a list of combinations of tiles to 
    string for easy-printing. *)
let string_of_all_combos combos = 
  let order = ref 0 in
  List.fold_left (fun acc combo -> 
      order := !order + 1; 
      acc ^ (string_of_int !order) ^ (string_of_combo combo) ^ "\n") 
    "" combos

(** [string_of_combo_emoji combo] convert a combination of tiles represented by a 
    list [combo] to an emoji. *)
let string_of_combo_emoji combo =
  List.fold_left 
    (fun acc tile -> acc ^ "  " ^ Tile.emoji_of_tile tile) "" combo

(** [string_of_current player] is the string representation of [player]'s tiles
    provided that player is the current player. *)
let string_of_current player =
  let id = Player.p_id player in
  let dark = Player.hand_tile_dark player in
  let light = Player.hand_tile_light player in
  "\nPlayer " 
  ^ string_of_int id 
  ^ "\n"
  ^ "Light hand: " ^ "\n"
  ^ string_of_combo_emoji light ^ "\n"
  ^ string_of_combo light ^ "\n"
  ^ "Dark hand: " ^ "\n"
  ^ string_of_combo_emoji dark ^ "\n"
  ^ string_of_combo dark ^ "\n\n"

(** [string_of_other player] is the string representation of [player]'s tiles
    provided that player is not the current player. *)
let string_of_other player =
  let id = Player.p_id player in
  let last_tile = 
    match Player.discard_pile player with
    | [] -> None
    | h :: s -> Some h in
  let light = Player.hand_tile_light player in
  "Player " 
  ^ string_of_int id 
  ^ "\n"
  ^ "Last discarded: " 
  ^ (fun tile_opt -> 
      match tile_opt with
      | Some last_tile -> Tile.string_of_tile last_tile
      | None -> "") last_tile 
  ^ "\n"
  ^ "Light hand: " 
  ^  string_of_combo light 
  ^  string_of_combo_emoji light ^ "\n"
  ^ "\n"

(** [display_player player] prints some of a player's information that is 
    appropriate to reveal at this turn. It varies in color and content 
    according to who is the current player. *)
let display_player state player =
  match player with
  | player when Player.p_id player = state.current_player_id ->
    ANSITerminal.(print_string [cyan] (string_of_current player))
  | player -> print_endline (string_of_other player)

(** [display_game state] prints the game at [state] *)
let display_game state = 
  List.map (display_player state) state.players

(* pretty-print helpers ends ************************************)

(** [after_chii current_player last_discarded hand_dark state] is the state of 
    game after [current_player] at [state] perform action chii. It calls 
    pre-defined functions to decide if the player is qualified to chii,
    calculates all combinations the player can build from chii, and update game 
    state.*)
let rec after_chii current_player last_discarded hand_dark state =
  let all_chii_combo = Tile.all_pos hand_dark last_discarded in
  ANSITerminal.(print_string [yellow] 
                  "You can chii the last discarded tile.\n");
  print_endline "Here are the options: ";
  print_endline (string_of_all_combos all_chii_combo);
  ANSITerminal.(print_string [yellow] 
                  "Which combo you would like to Chii? E.g. chii 1\n");
  print_endline ">>";
  chii_helper state last_discarded all_chii_combo current_player hand_dark

(** [chii_helper state last combos current_player hand_dark] displays all 
    update their hand according to the combo they command to build or skip this
    step if player decide not to chii. *)
and chii_helper state last combos current_player hand_dark = 
  try 
    match Command.parse (read_line ()) with
    | Chii n when 0 < n && n <= List.length combos -> 
      Player.chii_update_handtile n last current_player;
      ANSITerminal.(print_string [cyan] (string_of_current current_player));
      state
    | Chii n -> 
      ANSITerminal.(print_string [red] 
                      ("Please choose from the given option." ^ "\n" ^ ">>"));
      after_chii current_player last hand_dark state
    | Skip -> 
      ANSITerminal.(print_string [cyan] "Skipped!\n");  
      state
    | Discard (kind, number) -> 
      ANSITerminal.(print_string [red] 
                      ("You can't discard now."));
      after_chii current_player last hand_dark state 
    | Quit -> 
      ANSITerminal.(print_string [yellow] "Thank you for playing, bye!");
      { state with in_game = false }
  with
  | Command.Empty -> 
    print_endline "Empty command. Please try again.\n"; 
    after_chii current_player last hand_dark state 
  | Command.Malformed -> 
    print_endline "I don't understand this command. Please try again.\n";
    after_chii current_player last hand_dark state 

(** [after_draw current_player state] is the state of game after
    [current_player] at [state] draws a card. It takes the first tile from 
    the randomized pile and put it in player's hand. *)
let after_draw current_player state =
  let drawn_tile, wall =
    match state.wall_tiles with
    | [] -> raise Not_found
    | h :: t -> (h, t) in
  Player.draw_tile current_player drawn_tile;
  ANSITerminal.(print_string [yellow] 
                  ("Drawn tile: " ^ Tile.string_of_tile drawn_tile));
  ANSITerminal.(print_string [cyan] (string_of_current current_player));
  { state with wall_tiles = wall }

(** [after_discard current_player state] is the state of game after
    [current_player] at [state] discard a tile. If the player has riichi-ed,
    it automatically discard the tile they just drew. *)
let rec after_discard current_player state =
  print_endline {|Enter command to discard a tile. E.g. "discard Man 1"|};
  let drawn_tile = current_player |> Player.hand_tile_dark |> List.hd in
  let kind = Tile.get_kind drawn_tile in
  let number = Tile.get_number drawn_tile in
  match Player.state_r current_player with
  | true -> 
    ANSITerminal.(print_string [yellow] 
                    "You have riichi-ed, you can only discard the last drawn tile.");
    discard_helper current_player kind number state
  | false -> normal_discard current_player state

(** [normal_discard current_player state] is the routine where the player is
    not riichi-ed and can choose which tile to discard. *)
and normal_discard current_player state =
  try 
    match Command.parse (read_line ()) with
    | Discard (kind, number) -> 
      discard_helper current_player kind number state
    | Chii n -> 
      ANSITerminal.(print_string [red] 
                      ("You can't Chii now." ^ "\n" ^ ">>"));
      after_discard current_player state
    | Skip ->
      ANSITerminal.(print_string [red] 
                      ("You can't Skip now. Skip is for Chii" ^ "\n" ^ ">>"));
      after_discard current_player state
    | Quit -> 
      ANSITerminal.(print_string [yellow] "Thank you for playing, bye!");
      { state with in_game = false }
  with
  | Command.Empty -> 
    ANSITerminal.(print_string [red] "Empty command. Please try again.\n");
    after_discard current_player state
  | Command.Malformed -> 
    ANSITerminal.(print_string [red] 
                    "I don't understand this command. Please try again.\n");
    after_discard current_player state

(** [discars_helper current_player kind number state] discards the tile 
    <[kind] [number]> commanded by [current_player] if the tile is found in 
    their dark hand, and prompt them to re-enter command otherwise. *)
and discard_helper current_player kind number state = 
  let tile_opt = 
    Tile.find_tile kind number (Player.hand_tile_dark current_player) in
  match Player.discard_tile current_player tile_opt with
  | true -> state
  | false -> 
    ANSITerminal.(print_string [red] "You don't have this tile.\n");
    after_discard current_player state

(** [riichi_helper current_player to_get state] changes the state for 
    [current_player] to richii-ed and display what tiles are needed for them 
    to win. *)    
let riichi_helper current_player to_get state =
  ANSITerminal.(print_string [yellow] 
                  "You are riichi-ed!\n");
  print_endline "Just need these tiles to win: ";
  print_endline (string_of_combo to_get);
  print_endline (string_of_combo_emoji to_get);
  Player.riichi current_player;
  state

(** [after_check_richii current_player state] is the game state after we check
    if [current_player] can richii according to the rule. It calls 
    [riichi_helper] to perform the richii action and display more info for the 
    player. *)
let after_check_richii current_player state =
  let to_get = Player.check_riichi current_player in
  if to_get = [] then state
  else riichi_helper current_player to_get state

let ron_message = 
  "
 __ __  ____   __ ______   ___   ____   __ __  __ 
|  |  ||    | /  ]      | /   \\ |    \\ |  |  ||  |
|  |  | |  | /  /|      ||     ||  D  )|  |  ||  |
|  |  | |  |/  / |_|  |_||  O  ||    / |  ~  ||__|
|  :  | |  /   \\_  |  |  |     ||    \\ |___, | __ 
 \\   /  |  \\     | |  |  |     ||  .  \\|     ||  |
  \\_/  |____\\____| |__|   \\___/ |__|\\_||____/ |__|

Ron! Congratulations, you won the game!
"
let fireworks = 
  "
  .''.       
  .''.      .        *''*    :_\\/_:     . 
 :_\\/_:   _\\(/_  .:.*_\\/_*   : /\ :  .'.:.'.
.''.: /\ :   ./)\   ':'* /\ * :  '..'.  -=:o:=-
    :_\\/_:'.:::.    ' *''*    * '.\'/.' _\\(/_'.':'.'
   : /\ : :::::     *_\\/_*     -= o =-  /)\\    '  *
   '..'  ':::'     * /\\ *     .'/.\\'.   '
 *            *..*         :     *       *

  "

(** [afte_check_ron this_plr tile state] is the state after we check
    if [this_plr] has won. Game ends if a player won. *)
let after_check_ron this_plr tile state =
  let hand = Player.hand_tile_dark this_plr 
             @ Player.hand_tile_light this_plr 
             @ [tile] in
  let riichi_state = Player.state_r this_plr in 
  let comb_for_ron = Player.ini_comb hand riichi_state in
  match Player.ron comb_for_ron with
  | (true, yaku) -> 
    ANSITerminal.(print_string [yellow] ron_message);
    ANSITerminal.(print_string [yellow] 
                    ("You won with " ^ Player.string_of_yaku yaku ^ " yaku!"));
    ANSITerminal.(print_string [yellow] fireworks);
    { state with in_game = false }
  | (false, None) -> state
  | (false, yaku) -> state

(** [next_state state] is the game state after a player has played their turn.
    In a turn, a player will draw and discard tile, and if possible chii, 
    riichi, or win. *)
let rec next_state state = 
  ignore (display_game state);
  let last_plr_id = 
    if state.current_player_id <= 1 then 4
    else state.current_player_id - 1 in
  let last_player = 
    state.players |> extract [] (last_plr_id - 1) |> fst in
  let last_tile_opt =
    try Some (last_player |> Player.discard_pile |> extract [] 0 |> fst) with
    | Not_found -> None in
  let this_player = 
    state.players |> extract [] (state.current_player_id - 1) |> fst in
  let hand_dark = Player.hand_tile_dark this_player in
  match last_tile_opt with
  | Some last_tile when Tile.chii_legal hand_dark last_tile -> 
    chii_routine this_player last_tile hand_dark state 
  | Some last_tile -> 
    not_chii_routine this_player last_tile state 
  | None -> first_round_routine this_player last_player state 

(** [update_current_player state] is the state with current player updated to
    point to the next player. *)
and update_current_player state =
  let next_id = (state.current_player_id mod 4) + 1 in
  { state with current_player_id = next_id }

(** [check_for_tie state] check if the game is tie, which means the wall
    tiles have less than or equal to 4 tiles left but no player has won. *)
and check_for_tie state = 
  match state with
  | state when List.length state.wall_tiles <= 4 -> 
    ANSITerminal.(print_string [cyan] "Game tie!");
    { state with in_game = false }
  | state -> state     

(** [first_round_routine this_plr last_plr state] is the state after this turn 
    if [this_plr] is the first one in the whole game to make a move. This means
    that chii is not possible and we skip to draw. *)
and first_round_routine this_plr last_plr state =
  let state_after_draw = after_draw this_plr state in
  let drawn_tile = this_plr |> Player.hand_tile_dark |> List.hd in
  ron_discard_riichi_routine this_plr drawn_tile state_after_draw 

(** [chii_routine this_plr last_tile hand_dark state] is the state after this 
    turn if [this_plr] can perform chii action. In this case, the player cannot
    draw and we skip drawing and checking for ron to discarding tile. If the
    player choose not to chii, we jump to [not_chii_routine]. *)
and chii_routine this_plr last_tile hand_dark state = 
  match after_check_ron this_plr last_tile state with
  | state' when state'.in_game ->
    let light0 = Player.hand_tile_light this_plr in
    let state1 = after_chii this_plr last_tile hand_dark state' in
    if List.length (Player.hand_tile_light this_plr) 
       <> List.length light0 then 
      state1
      |> after_discard this_plr 
      |> after_check_richii this_plr
      |> update_current_player
    else not_chii_routine this_plr last_tile state'
  | state' -> state'

(** [not_chii_routine this_plr last_tile state] is the state after 
    this turn if [this_plr] can not chii. This means they will draw, be checked
    for ron, and be checked for riichi. *)
and not_chii_routine this_plr last_tile state =
  match after_check_ron this_plr last_tile state with
  | state' when state'.in_game -> begin
      let state1 = after_draw this_plr state' in
      let drawn_tile = this_plr |> Player.hand_tile_dark |> List.hd in
      ron_discard_riichi_routine this_plr drawn_tile state1
    end
  | state' -> state'

(** [ron_discard_riichi_routine this_plr tile state] is a subroutine to check 
    for ron in between of a turn. *)
and ron_discard_riichi_routine this_plr tile state = 
  match after_check_ron this_plr tile state with
  | state' when state'.in_game -> begin
      let state1 = after_discard this_plr state' in
      match check_for_tie state1 with
      | state' when state'.in_game ->
        state'
        |> after_check_richii this_plr
        |> update_current_player
      | state' -> state'
    end
  | state' -> state'
