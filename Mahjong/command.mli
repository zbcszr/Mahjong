(** 
   Parsing of player commands.

   This module represents the player command that is decomposed into 
   either [Discard], [Chii], [Ron], or [Quit] and possibly a tile.
*)

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

(** [parse str] parses a player input into a typed [command]. The first word of 
    [str] becomes the cammand verb. The rest of the [str], if any and valid, 
    become the tile. 

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.)

    Raises: [Empty] if [str] is the empty string or contains only spaces.

    Raises: [Malformed] if the command is malformed, which means that the input
    verb is none of [Discard], [Chii], [Ron], or [Quit].
*)
val parse : string -> command
