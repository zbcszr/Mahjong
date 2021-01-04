(** [play_game ()] runs the game. It repetitively calls [next_state state] to
    update game state at each turn. *)
let play_game () =
  let init = Game.init_state () in
  let state = ref (Game.make_game init) in 
  while Game.is_in_game !state do begin
    print_endline ">>";
    state := Game.next_state !state;
  end done

let title = 
  "
   +-------------------------------------------------------------------------------------+
  |                                                                                     |
  |                                                                                     |
  |                 ___ ___   ____  __ __  ____   ___   ____    ____                    |
  |                |   |   | /    ||  |  ||    | /   \\ |    \\  /    |                   |
  |                | _   _ ||  o  |+  |  |+__  ||     ||  _  ||   __|                   |
  |                |  \\_/  +|     +|  _  |__|  +|  O  +|  |  +|  |  |                   |
  |                |   |   |+  _  |+  |  /  |  |+     |+  |  |+  |_ |                   |
  |                |   |   ||  |  ||  |  \\  `  ||     ||  |  ||     |                   |
  |                |___|___||__|__||__|__|\\____j \\___/ |__|__||___,_|                   |
  |                                                                                     |
  |                                                                                     |
  |                                                                                     |
  |                                                                                     |
  |                                   +-+ +--+ +--+                                     |
  |                                   | S T A R T |                                     |
  |                                   ^-----------^                                     |
  |                                                                                     |
  ++                                                                                   ++
   +------------------------------------------------------------------------------------+


"

let welcome_message = "\n\nWelcome to the Text-based Richii Mahjong!"

let guide_message = 
  "
Please refer to the command examples below:
Normal tile: Man 1
Wind tiles: 
  Wind 1 = East
  Wind 2 = South
  Wind 3 = West
  Wind 4 = North
Dragon tiles:
  Dragon 1 = Red Dragon
  Dragon 2 = Green Dragon
  Dragon 3 = White Dragon
Discard a tile: discard Man 1
Chii (we will give you # of options to choose from): chii 1
Skip a Chii action: skip
Quit the game: quit

Note that all commands are case sensitve. Enjoy! XD
"

(** [main ()] prints out starting message and ask user to enter command to
    start the game. *)
let main () = 
  ANSITerminal.(print_string [yellow] title);
  ANSITerminal.(print_string [cyan] welcome_message);
  print_endline guide_message;
  ANSITerminal.(print_string [yellow] 
                  "Please enter 'start' to start the game.");
  print_endline ">>";
  match read_line () with
  | "start" -> play_game ()
  | "quit" -> ANSITerminal.(print_string [cyan] "Bye!")
  | command -> begin
      let message = 
        "You can't start game with" 
        ^ command 
        ^ {|, please restart the game with "make play" and enter start this time.|} 
      in
      ANSITerminal.(print_string [red] message);
    end

let () = main ()
