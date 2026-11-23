open Island_survival

type turn =
  | Continue of Game.t
  | Stop

(** [finish game] returns [()] after printing the time survived. *)
let finish game = print_endline (Terminal.render_summary game)

(** [run_action game action] returns the next turn after applying [action] and
    printing its result. A failed action keeps the current game. *)
let run_action game action =
  match Game.step game action with
  | Error reason ->
      print_endline (Terminal.render_error reason);
      Continue game
  | Ok updated ->
      print_endline (Terminal.render_success action);
      Continue updated

(** [run_command game command] returns the next turn after handling [command].
*)
let run_command game = function
  | Terminal.Help ->
      print_endline Terminal.help;
      Continue game
  | Terminal.Status -> Continue game
  | Terminal.Quit ->
      finish game;
      Stop
  | Terminal.Action action -> run_action game action

(** [run_line game line] returns the next turn after parsing [line]. Invalid
    input prints guidance and keeps the current game. *)
let run_line game line =
  match Terminal.parse line with
  | Error message ->
      print_endline message;
      Continue game
  | Ok command -> run_command game command

(** [loop game] returns [()] after reading commands until death, quit, or
    end-of-file. It prints the game state before each prompt. *)
let rec loop game =
  print_endline (Terminal.render_state game);
  if not (Game.is_alive game) then finish game
  else (
    print_string "> ";
    flush stdout;
    match read_line () with
    | exception End_of_file ->
        print_newline ();
        finish game
    | line -> (
        match run_line game line with
        | Continue updated -> loop updated
        | Stop -> ()))

let () = loop (Game.initial ())
