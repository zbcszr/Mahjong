(** 
   Representation of dynamic tile data.

   This module is a representation of each tile, including the kinds, ids, 
   and discardable status. It handles tile related actions such as ck_eq, 
   ck_tri, ck_seq, and sort.
*)

(** The abstract type of values representing a tile. *)
type t

(** Type id represents the [id] of a tile. *)
type id = int

(** Type kind represents the [kind] of a tile. *)
type kind = Man | Pin | Sou | Dragon | Wind

(** Raised when an unknown tile is played. *)
exception UnknownTile of id

(** [update_status tile] is the discardable status of a tile *)
val update_status : t -> unit

(** [get_id tile] gets the id of [tile] *)
val get_id : t -> id

(** [get_kind tile] gets the kind of [tile] *)
val get_kind : t -> kind

(** [get_number tile] gets the number of [tile] *)
val get_number : t -> int

(** [find_tile kind number] finds the tile represented by [kind] [number]. 
    i.e. given the kind of the tile and the number on the tile, find the 
    complete tile representation among a given list of tiles. In case of 
    duplicated kind and number, find the first occurence.
    [find_tile kind number] = [Some tile] if found, [None] if not found. *)
val find_tile : kind -> int -> t list -> t option

(** [dp tile] display [tile] *)
val dp : t -> unit

(** [string_of_tile tile] is a tring representation of [tile]. *)
val string_of_tile : t -> string

(** [construct id kind num discarded] init a tile *)
val construct : id -> kind -> int -> bool -> t

(** [sim_construct kind number] initializes a tile from its [kind] and 
    [number] *)
val sim_construct: kind -> int -> t

(** [ck_n_o tile] is whether [tile] is of number 1 or 9. *)
val ck_n_o : t -> bool

(** [ck_adj t1 t2] checks if two tile are adjasent means that it has potential to be
    a sequence. *)
val ck_adj : t -> t -> bool

(** [ck_eq t1 t2] checks if two tile is same kind and same number*)
val ck_eq : t -> t -> bool

(** [ck_seq t1 t2 t3]check if three tiles form a sequence  *)
val ck_seq: t -> t -> t-> bool

(** [ck_tri t1 t2 t3] checks if three tile has same kind and number*)
val ck_tri : t -> t -> t-> bool

(** [filter_kind kind lst] gives all tiles with specific kind *)
val filter_kind : kind -> t list -> t list

(** [sort lst] sorts a list of tiles based on kind and number*)
val sort : t list -> t list

(** [chii_legal lst t] checks if user is able to chii *)
val chii_legal : t list -> t -> bool

(** [all_pos t_list tile] returns all possible triplet and sequence *)
val all_pos : t list -> t -> t list list

(** [emoji_of_tile tile] returns the emoji representation of [tile]*)
val emoji_of_tile : t -> string