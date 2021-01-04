(** 
    Representation of a player.

    This module represents a player with id. It handles player related actions
    such as draw_tile, discard_tile, chii and ron.
*)

(** The abstract type representing player. *)
type t

(** p_id is the identifier of different player.
    Different player should have different id*)
type id = int

(** [p_id t] gets the id of player [t]*)
val p_id : t -> id

(** [state_r t] gets riichi state of player [t]*)
val state_r : t -> bool

(** [state_c t] gets chii state of player [t]*)
val state_c : t -> bool

(** [hand_tile_light t] gets all the light hand tiles of the player [t]*)
val hand_tile_light : t -> Tile.t list

(** [hand_tile_dark r] gets all the dark hand tiles of the player [t]*)
val hand_tile_dark : t -> Tile.t list

(** [discard_pile t] gets the list of discarded tiles of the player [t]*)
val discard_pile : t -> Tile.t list

(** [draw_tile] puts tile into the player's dark tiles. *)
val draw_tile : t -> Tile.t -> unit

(** [discard_tile t tile_opt] play a tile with id in t, return false if the tile
    that player wants to discard is not in their hand tiles. *)
val discard_tile: t -> Tile.t option -> bool

(** [chii_update_handtile int tile player] updates handtile and state_c of
    [player]. [int] is the n-th option [player] chose among all options,
    [tile] is the tile [player] wants to chii *)
val chii_update_handtile : int -> Tile.t -> t -> unit

(** [check_riichi t] checks if the player [t] can riichi. 
    return an empty list if the player cannot riichi*)
val check_riichi: t -> Tile.t list

(** [riichi t] changes the status from normal to riichi, given the player is 
     legal to riichi *)
val riichi: t -> unit

(** [d_list tile_lst] displays the tiles of current player *)
val d_list: Tile.t list -> unit

(** [init_player id richii chii light dark discard] constructor for a player *)
val init_player: int -> bool -> bool -> Tile.t list -> Tile.t list -> 
  Tile.t list -> t

(** The type [comb] is combination of information that is used to determine if 
    a player has a winning hand. *)
type comb

(** [ini_com tile_lst] initializes a [comb] for [tile_list] that is to be used
    in determining if this list of tile consists a winning hand. *)
val ini_comb: Tile.t list -> bool -> comb

(** [ini_info tile_lst info] initialize a (Tile.t * int) list where fst element
    of the tuple is a tile, snd element of tuple is the number of same tile user 
    has in handtile. *)
val ini_info: Tile.t list -> (Tile.t * int) list -> (Tile.t * int) list

(** [print_info info] prints info for display *)
val print_info: (Tile.t * int) list -> unit

(** [check_triplet comb] checks if user is able to ron*)
val check_triplet : comb -> bool

(** The type [yaku] represents the types of yaku player can achieve *)
type yaku = Riichi | Tanyao | Hunyise | Dragontriplet | Seven_Pairs | Pinfu 
          | None

(** [string_of_yaku yaku] is the string representation of a yaku. *)
val string_of_yaku : yaku -> string

(** [ron comb] returns a tuple. First element indicates if the player can ron or
    not, the second indicates one of the yakus that the helps user to ron. *)
val ron : comb -> (bool * yaku)

(** [ini_comb_yaku pair triplet seq bool] returns a comb initialied with
    [pair], [triplet], [seq], and [bool]*)
val ini_comb_yaku: 
  Tile.t list -> Tile.t list list -> Tile.t list list -> bool -> comb
