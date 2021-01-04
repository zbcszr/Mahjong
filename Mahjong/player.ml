(** type [id] is the id of player  *)
type id = int

(** [handt] is the type for player's hand tile *)
type handt = {
  mutable light: Tile.t list;
  mutable dark: Tile.t list;
}

type player = {
  (** id of the player*)
  id : int;
  (** State_r tells about state of current player.
      If the player have already riichi,state_r is true, otherwise false *)
  mutable state_r : bool;
  (** State_c tells about state of current player.
      If the player have done action chii, this state is true, otherwise false*)
  mutable state_c : bool;
  (** the tile player currently have. Number varies from 13 to 14 *)
  hand_tile : handt;
  (** tiles that played by this player*)
  mutable discard_pile : Tile.t list;
}

(** The abstract type representing player. *)
type t = player

(** p_id is the identifier of different player.
    Different player should have different id*)
let p_id t = t.id

(** [state_r t] gets riichi state of player [t]*)
let state_r t = t.state_r

(** [state_c t] gets chii state of player [t]*)
let state_c t = t.state_c

(** [hand_tile_light t] gets all the light hand tiles of the player [t]*)
let hand_tile_light t = t.hand_tile.light

(** [hand_tile_dark r] gets all the dark hand tiles of the player [t]*)
let hand_tile_dark t = t.hand_tile.dark

(** [discard_pile t] gets the list of discarded tiles of the player [t]*)
let discard_pile t = t.discard_pile

(** [draw_tile player tile] draws [tile] from wall tiles, put it in 
    [player]'s hand, and sort the new hand tiles. *)
let draw_tile player tile =
  (* player.hand_tile.dark <- (Tile.sort ([tile] @ player.hand_tile.dark)) *)
  player.hand_tile.dark <- [tile] @ player.hand_tile.dark

(* [remove_fst_occur lst tile acc] removes first occurence of [tile] from 
   [lst] *)
let rec remove_fst_occur lst tile acc = 
  match lst with
  | [] -> lst 
  | h :: t -> begin
      if Tile.ck_eq h tile then acc @ t else 
        remove_fst_occur t tile (acc @ [h])
    end

(* [remove_tile_lst tlist pile] removes [tlist] from [pile] *)
let rec remove_tile_lst tlist pile = 
  match tlist with
  | [] -> pile
  | h :: t -> begin
      remove_tile_lst t (remove_fst_occur pile h [])
    end

(** [discard_tile t tile_opt] play a tile with id in t, return false if the 
    tile that player wants to discard is not in their hand tiles. *)
let discard_tile player tile_opt =
  let handt = player.hand_tile.dark in
  let discardt = player.discard_pile in 
  match tile_opt with
  | None -> false
  | Some tile -> begin 
      Tile.update_status tile;
      player.hand_tile.dark <- Tile.sort (remove_tile_lst [tile] handt);
      player.discard_pile <- [tile] @ discardt;
      true
    end

(** [d_list l] displays a list [l] of tile *)
let d_list l = 
  List.iter Tile.dp l

(** [init_player id richii chii light dark discard] is the constructor of 
    player type*)
let init_player id richii chii light dark discard =
  let handt = {
    light = light;
    dark = dark; 
  } in 
  { id = id;
    state_r = richii;
    state_c = chii;
    hand_tile = handt;
    discard_pile = discard;
  }

(** [combine t] returns a list of tiles that is the combination of 
    light and dark *)
let combine t = 
  let hand = t.hand_tile in 
  Tile.sort hand.light @ hand.dark

(** [get_ele lst n] gets the nth tile in form of a list in user's combined hand 
    tile. *)
let rec get_ele lst n =
  match lst with
  | [] -> []
  | h :: t -> begin
      if n <= 0 then h
      else get_ele t (n - 1)
    end

(* [chii_update_handtile int tile player] updates player's dark, light handtile,
   and state_c to true.
   [int] is the i-th option in all possible chii-combination, the first element
   in a list is the 1st option. Return 1st ele if int <= 0
   [tile] is the tile [player] wants to chii. 
   Precondition:[player] wants to and is eligible to chii tiles.*)
let chii_update_handtile int tile player = 
  let hand_tile = player.hand_tile in
  let dark = hand_tile.dark in 
  let light = hand_tile.light in
  let all_pos = Tile.all_pos dark tile in 
  let picked = get_ele all_pos (int - 1) in
  hand_tile.dark <- remove_tile_lst picked (tile :: dark);
  hand_tile.light <- picked @ light;
  player.state_c <- true;
  ()

type comb = {
  (**  Players handtile in the form of (Tile, number_of_tiles) list. 
       For example: [Sou1; Sou2; Sou2] is stored as [(Sou1, 1); (Sou2, 2)]*)
  info: (Tile.t * int) list;
  (** the pair in user's handtile*)
  pair: Tile.t list;
  (** the triplets in user's handtile*)
  triplet: Tile.t list list;
  (** the sequence in user's handtile*)
  seq: Tile.t list list;
  (** indicate if user has riichied or not*)
  riichied: bool;
} 

(* [generate_info h left_acc right_acc] find h in acc, 
   if exist >> (h, count+1),
   if not, append (h,1) on acc *)
let rec generate_info h left_acc right_acc =
  match right_acc with 
  | [] -> left_acc @ [(h, 1)]
  | (tile, count) :: t -> begin
      if Tile.ck_eq h tile then left_acc @ [tile, count + 1] @ t
      else generate_info h (left_acc @ [(tile, count)]) t
    end

(* [ini_info lst acc] takes in a list [lst] of tiles and return a list [acc] 
   of tuples [(tile, count)]*)
let rec ini_info lst acc=
  match lst with
  | [] -> acc
  | h :: t -> 
    ini_info t (generate_info h [] acc)

(** [get_info h info] gets the count of a certain tile [h]*)
let rec get_info h info=
  match info with
  | [] -> 0
  | (tile, count) :: t -> begin
      if Tile.ck_eq h tile then count
      else get_info h t
    end

(** [update_info info] clears out tile from info with count = 0 *)
let update_info info = 
  let rec update info acc = 
    match info with
    | [] -> acc 
    | (tile, int) :: t -> begin
        if int <= 0 then update t acc
        else update t (acc @ [(tile, int)])
      end
  in
  update info []

(* [rem_l int lst] remove first [int]-th elements from [lst] *)
let rec rem_l int lst=
  if int = 0 then lst 
  else begin
    match lst with
    | [] -> failwith "not right rem_l"
    | h :: t -> rem_l (int - 1) t
  end

(* [rem_info_c int h left_acc right_acc] returns a list with 
   removed [int] counts from tuple curresponding to tiile [h]*)
let rec rem_info_c int h left_acc right_acc =
  match right_acc with 
  | [] -> left_acc
  | (tile, count) :: t -> begin
      if Tile.ck_eq h tile then left_acc @ [tile, count - int] @ t
      else rem_info_c int h (left_acc @ [(tile, count)]) t
    end

(* [rem_li_seq n seq lst left_list] removes the first sequence from a sorted 
   lst. Requires: Seq must be a sequence *)
let rec rem_li_seq n seq lst left_list= 
  if n = 0 then left_list @ lst
  else 
    match lst with
    | [] ->failwith "Not right input"
    | h :: t -> 
      begin match seq with 
        | [] ->failwith "Not right input"
        | x :: y -> if (Tile.ck_eq x h)
          then rem_li_seq (n - 1) y t left_list
          else rem_li_seq n lst t (left_list @ [h])
      end

(* [remove_zero_count lst] removes all tuples with count = 0 *)
let remove_zero_count lst = 
  List.filter (fun (tile, count) -> count <> 0) lst

(* [remove_info_seq int info left_acc] drecrement count for first three tuple 
   in info. *)
let rec remove_info_seq int info left_acc = 
  if int = 0 then (remove_zero_count left_acc @ info)
  else begin
    match info with
    | [] -> failwith "not right remove info seq"
    | (tile, count) :: t -> 
      remove_info_seq (int - 1) t (left_acc @ [(tile, count - 1)])
  end

(* [get_first_three int info acc] returns the first three tiles in info list*)
let rec get_first_three int (info : (Tile.t * int) list) acc = 
  if int = 0 then acc 
  else begin
    match info with
    | [] -> failwith "not right get_first_3"
    | (tile, count) :: t -> get_first_three (int - 1) t (acc @ [tile])
  end

(** [print_info info] prints info for display *)
let rec print_info info = 
  match info with
  | [] -> ()
  | (tile, int) :: t -> Tile.dp tile; print_endline (" " ^ string_of_int int);
    print_info t

(* [fill tile num acc] returns [num] number of [tile] in a list*)
let rec fill tile num acc = 
  if num = 0 then acc 
  else fill tile (num - 1) (tile :: acc)

(* [flatten_info info] flatten info to a list of tiles,
   For example: flatten [(Man1, 2)] [] = [Man1; Man1]*)
let flatten_info info =
  let rec helper info acc =  
    match info with
    | [] -> acc
    | (tile, num) :: t -> helper t ((fill tile num []) @ acc)
  in helper info []

(* check if [tile] is in list yaojiu. Not like List.mem bc need to use self-
   defined check_equal but not physical equality*)
let tanyao_helper tile =
  let yaojiu =
    (Tile.sim_construct Man 1) :: (Tile.sim_construct Man 9) ::
    (Tile.sim_construct Sou 1):: (Tile.sim_construct Sou 9) ::
    (Tile.sim_construct Pin 1) :: (Tile.sim_construct Pin 9) ::
    (Tile.sim_construct Dragon 1) :: (Tile.sim_construct Dragon 2) ::
    (Tile.sim_construct Dragon 3) :: (Tile.sim_construct Wind 1) ::
    (Tile.sim_construct Wind 2) :: (Tile.sim_construct Wind 3) ::
    [(Tile.sim_construct Wind 4)] in
  match List.find_opt (fun x -> Tile.ck_eq tile x) yaojiu with
  | None -> false
  | Some _ -> true

(** [is_tanyao new_l] is true if player satisfies duanyaoji: 
    cannot be number 1, number 9, wind or dragon *)
let is_tanyao new_l =
  not (List.exists Tile.ck_n_o new_l)

(* [is_hunyise new_l] is true if player satisfies hunyise: 
   a hand with tiles from only one of the three number tiles (Man Pin Sou),
   and wind tiles and dragon tiles*)
let is_hunyise new_l =
  let man = List.length (Tile.filter_kind Tile.Man new_l) in 
  let sou = List.length (Tile.filter_kind Tile.Sou new_l) in 
  let bin = List.length (Tile.filter_kind Tile.Pin new_l) in 
  (man = 0 && sou = 0) || (man = 0 && bin = 0) || (sou = 0 && bin = 0) 

(** [is_dragons new_tri] is true if user has hand tile with triple of dragon *)
let rec is_dragons info = 
  match info with 
  | [] -> false 
  | (tile, int) :: t -> begin 
      (Tile.ck_eq (Tile.sim_construct Dragon 1) tile && int > 2)
      || (Tile.ck_eq (Tile.sim_construct Dragon 2) tile && int > 2)
      || (Tile.ck_eq (Tile.sim_construct Dragon 3) tile && int > 2)
      || is_dragons t
    end

(** return true if the combination satisfy pinfu - 
    all sequence and a non-dragon pair - false otherwise. *)
let is_pinfu new_seq pair =
  match pair with 
  | [] -> false
  | tile :: t -> 
    let k = Tile.ck_eq (Tile.sim_construct Dragon 1) tile 
            || Tile.ck_eq (Tile.sim_construct Dragon 2) tile 
            || Tile.ck_eq (Tile.sim_construct Dragon 3) tile in 
    not k && List.length new_seq = 12

(** [is_seven_pairs comb] if true if user satisfy seven_pairs, false otherwise
*)
let is_seven_pairs comb_info = 
  List.fold_left (fun sum item -> sum + snd item) 0 comb_info = 14
  && List.fold_left (fun acc item -> 
      acc && (snd item mod 2 = 0)) true comb_info

(** [check_yaku comb] checks if this combination of tile satisfies at least one
    yaku *)
let check_yaku comb = 
  let new_l = flatten_info comb.info in  
  comb.riichied 
  || is_tanyao new_l 
  || is_hunyise new_l 
  || is_dragons comb.info
  || is_seven_pairs comb.info

(** The type [yaku] represents the types of yaku player can achieve *)
type yaku = Riichi | Tanyao | Hunyise | Dragontriplet | Seven_Pairs | Pinfu 
          | None

(** [string_of_yaku yaku] takes in [yaku] return the string of it *)
let string_of_yaku yaku = 
  match yaku with
  | Riichi -> "Riichi"
  | Tanyao -> "Tanyao"
  | Hunyise -> "Hunyise"
  | Dragontriplet -> "Dragon Triplet"
  | Seven_Pairs -> "Seven Pairs"
  | Pinfu -> "Pinfu"
  | None -> ""

(** [check_sequence lst] checks if a given list of tiles [lst] is forms a seq*)
let check_sequence lst = 
  match lst with
  | t1 :: t2 :: t3 :: []-> Tile.ck_seq t1 t2 t3
  | _ -> failwith "not right input for check sequence"

(** Check if a combination of tiles can Ron 
    - [check_triplet comb] is true if user can ron;
    - [check_triplet_2 comb] is true if there is a valid triplet, modifies 
      info, add triplet to comb.triplet, call check_pair
    - [check_pair comb] is true if there is a valid pair, modifies info and add
      pair into comb.pair, check_seq
    - [check_seq comb] is true if there is a valid seq, modifies info, add seq
      to comb.seq, call check_triplet
*)
let rec check_triplet comb = 
  if List.length comb.info = 0 
  then
    List.length comb.pair = 2 
    && (List.length comb.triplet + List.length comb.seq = 4)
    || List.length comb.pair = 14
  else check_triplet_2 comb

and check_triplet_2 comb =
  match comb.info with
  | [] -> failwith "not right triple"
  | h :: t -> 
    let new_tile = fst h in
    let new_info = update_info (rem_info_c 3 new_tile [] comb.info) in
    let new_k = [new_tile; new_tile; new_tile] :: comb.triplet in
    get_info new_tile comb.info > 2 
    && check_triplet { comb with info = new_info; triplet = new_k } 
    || check_pair comb

and check_pair comb = 
  match comb.info with 
  | [] -> failwith "not right pair"
  | h :: t -> 
    let new_tile = fst h in
    let new_info = update_info (rem_info_c 2 new_tile [] comb.info) in
    let new_p = [new_tile; new_tile] @ comb.pair in begin
      get_info new_tile comb.info > 1 
      && check_triplet { comb with info = new_info; pair = new_p }
      || check_seq comb
    end

and check_seq comb = 
  match comb.info with 
  | [] -> failwith "not right seq"
  | h :: t ->  
    let len_greater_than_2 = List.length comb.info > 2 in
    if len_greater_than_2 then
      let new_info = update_info (remove_info_seq 3 comb.info []) in
      let new_s = (get_first_three 3 comb.info []) :: comb.seq in
      check_sequence (get_first_three 3 comb.info []) 
      && check_triplet { comb with info = new_info; seq = new_s }
    else false

(** [ron comb] returns a tuple. First element indicates if the player can ron or
    not, the second indicates one of the yakus that the helps user to ron. *)
let ron comb =
  let new_l = flatten_info comb.info in 
  if check_triplet comb && check_yaku comb then begin
    if comb.riichied then (true, Riichi)
    else if is_dragons comb.info then (true, Dragontriplet)
    else if is_seven_pairs comb.info then (true, Seven_Pairs)
    else if is_tanyao new_l then (true, Tanyao)
    else if is_hunyise new_l then (true, Hunyise)
    else (false, None)
  end
  else (false, None) 

(** [ini_comb lst bool] initialize a comb which work as information of hand tile
    [bool] represents whether the player have riichied*)
let ini_comb lst bool = {
  pair = [];
  triplet = [];
  info = ini_info (Tile.sort lst) [];
  seq = [];
  riichied = bool;
}

(** [ini_comb_yaku pair tri seq bool] initialize a comb which work as 
    information of hand tile. [bool] represents whether the player have 
    riichied*)
let ini_comb_yaku pair triplet seq bool = {
  pair = pair;
  triplet = triplet;
  info = [];
  seq = seq;
  riichied = bool;
}

(** [riichi player] change player's state to riichi-ed *)
let riichi player =
  if player.state_r then failwith " player already riichi ed"
  else 
    player.state_r <- true;
  ()

(** [generate_n kind n acc] given the kind, generate 1-9 tile of this kind *)
let rec generate_n kind n acc = 
  if n > 0 then begin 
    let new_t= Tile.construct 1 kind n false in
    generate_n kind (n - 1) (new_t :: acc)
  end
  else acc

(** [generate tiles] generates a list of all differnet tile *)
let generate_tiles = 
  let rec generate lst acc = 
    match lst with
    | [] -> acc
    | h :: t -> begin
        match h with
        | Tile.Pin | Tile.Man | Tile.Sou -> 
          generate t (acc @ generate_n h 9 [])
        | Tile.Wind -> generate t (acc @ generate_n h 4 [])
        | Tile.Dragon -> generate t (acc @ generate_n h 3 [])
      end
  in generate [Tile.Pin; Tile.Man; Tile.Sou; Tile.Wind; Tile.Dragon] []

(** [check_r_help hand lst acc bol ] [hand] represent hand tile, [list] 
    represent various tile type, [acc] represent feasible riichi tile 
    helper function for check_riichi*)
let rec check_r_help hand lst acc bol = 
  match lst with
  | [] -> acc
  | h ::t -> begin
      let n_hand = Tile.sort (h :: hand) in 
      let n_comb = ini_comb n_hand bol in
      if (check_triplet n_comb) then
        check_r_help hand t (h :: acc) bol
      else 
        check_r_help hand t acc bol
    end

(** [check_riichi player] return a list of tile that the [player] needs to 
    richii *)
let check_riichi player =
  if (player.state_c || player.state_r) then []
  else
    let handtile = combine player in
    let lst = generate_tiles in
    check_r_help handtile lst [] false
