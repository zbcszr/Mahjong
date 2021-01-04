(** The type [command] represents a player command that is decomposed into 
    either [Discard], [Chii], [Ron], or [Quit] and possibly a tile. *)
type command =
  | Discard of (Tile.kind * int)
  | Chii of int
  | Skip
  | Quit

(** Exception raised when an empty command is parsed. *)
exception Empty

(** Exception raised when a malformed command is parsed. *)
exception Malformed

(** [kind_of_string str] is the kind of a tile given the string of a tile*)
let kind_of_string str = 
  match str with
  | "Man" -> Tile.Man
  | "Pin" -> Tile.Pin
  | "Sou" -> Tile.Sou
  | "Dragon" -> Tile.Dragon
  | "Wind" -> Tile.Wind
  | _ -> raise (Malformed)

(** [parse_tile lst i acc] parses a list of tile-words into a 
    (Tile.kind * int) tuple. Requires: [lst] contains two string items: the 
    type of the tile and a number. *)
let rec parse_tile lst i acc = 
  match lst with
  | [] -> acc
  | h :: t -> begin
      if i = 0 then 
        let (kind, num) = acc in
        parse_tile t (i + 1) (kind_of_string h, num)
      else let (kind, num) = acc in
        parse_tile t (i + 1) (kind, int_of_string h)
    end

(** [parse_number lst] parses the number input in string to an int contained 
    in a command-token list. *)
let parse_number lst =
  match lst with
  (* this branch should never be reached as it's ruled out in [parse]*)
  | [] -> 0 
  | h :: t -> int_of_string h

(** [parse str] parses a command input [str] in string format into commands 
    that are recognizable to the system. *)
let parse str =
  let str_lst = String.split_on_char ' ' str in
  let clean_lst = List.filter (fun item -> item <> "") str_lst in
  match clean_lst with
  | [] -> raise Empty
  | h :: t when h = "discard" && t <> [] -> Discard (parse_tile t 0 (Man, 0))
  | h :: t when h = "chii" && t <> [] -> Chii (parse_number t)
  | h :: t when h = "skip" -> Skip
  | h :: t when h = "quit" -> Quit
  | h :: t -> raise Malformed
