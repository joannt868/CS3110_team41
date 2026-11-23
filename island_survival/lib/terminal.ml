type command =
  | Action of Game.action
  | Status
  | Help
  | Quit

(** [words line] returns lowercase words separated by spaces in [line]. *)
let words line =
  String.split_on_char ' ' (String.lowercase_ascii (String.trim line))
  |> List.filter (fun word -> word <> "")

let unknown_command =
  Error "Unknown command or arguments. Type help to see your options."

(** [parse_move target] returns a move command for a known [target]. *)
let parse_move = function
  | [ "beach" ] -> Ok (Action (Game.Move World.Beach))
  | [ "forest" ] -> Ok (Action (Game.Move World.Forest))
  | _ -> unknown_command

(** [parse_gather target] returns a gather command for a known [target]. *)
let parse_gather = function
  | [ "stone" ] -> Ok (Action (Game.Gather Resource.Stone))
  | [ "water" ] | [ "fresh"; "water" ] ->
      Ok (Action (Game.Gather Resource.Fresh_water))
  | [ "wood" ] -> Ok (Action (Game.Gather Resource.Wood))
  | [ "berries" ] -> Ok (Action (Game.Gather Resource.Berries))
  | _ -> unknown_command

(** [parse_craft target] returns a craft command for a known [target]. *)
let parse_craft = function
  | [ "pickaxe" ] -> Ok (Action (Game.Craft Crafting.Pickaxe))
  | [ "shelter" ] -> Ok (Action (Game.Craft Crafting.Shelter))
  | _ -> unknown_command

(** [parse line] returns a command or a short usage message. *)
let parse line =
  match words line with
  | [ "help" ] -> Ok Help
  | [ "status" ] -> Ok Status
  | [ "quit" ] -> Ok Quit
  | [ "eat" ] -> Ok (Action Game.Eat)
  | [ "drink" ] -> Ok (Action Game.Drink)
  | [ "rest" ] -> Ok (Action Game.Rest)
  | "move" :: target -> parse_move target
  | "gather" :: target -> parse_gather target
  | "craft" :: target -> parse_craft target
  | [] -> Error "Enter a command. Type help to see your options."
  | _ -> unknown_command

let help =
  String.concat "\n"
    [
      "Commands:";
      "  move beach|forest         Move to a connected location (2 hours)";
      "  gather stone|water|wood|berries  Collect one unit (1 hour)";
      "  craft pickaxe|shelter     Make one item (2 hours)";
      "  eat                       Eat one berry (1 hour)";
      "  drink                     Drink one fresh water (1 hour)";
      "  rest                      Rest for 8 hours";
      "  status                    Show the current state (no time)";
      "  help                      Show these commands (no time)";
      "  quit                      End the run (no time)";
    ]

(** [item_name item] returns the display name for [item]. *)
let item_name = function
  | Crafting.Pickaxe -> "Pickaxe"
  | Crafting.Shelter -> "Shelter"

(** [show_list render values] returns comma-separated [values], or [none] when
    the list is empty. *)
let show_list render values =
  match values with
  | [] -> "none"
  | _ -> String.concat ", " (List.map render values)

(** [show_resources entries] returns resource names and quantities. *)
let show_resources entries =
  show_list
    (fun (resource, count) ->
      Printf.sprintf "%s: %d" (Resource.name resource) count)
    entries

(** [inventory_entries player] returns the counts of all four resource kinds. *)
let inventory_entries player =
  let stock = Player.inventory player in
  List.map
    (fun resource -> (resource, Resource.quantity stock resource))
    [ Resource.Stone; Resource.Fresh_water; Resource.Wood; Resource.Berries ]

(** [days_and_hours total_hours] returns completed days and remaining hours. *)
let days_and_hours total_hours = (total_hours / 24, total_hours mod 24)

(** [render_state game] returns a complete status frame. *)
let render_state game =
  let player = Game.player game in
  let location = Player.location player in
  let days, hour = days_and_hours (Game.elapsed_hours game) in
  let routes = show_list World.name (World.destinations location) in
  let resources = show_resources (World.resources (Game.world game) location) in
  let inventory = show_resources (inventory_entries player) in
  let items = show_list item_name (Game.items game) in
  String.concat "\n"
    [
      Printf.sprintf "Day %d, Hour %d" (days + 1) hour;
      "Location: " ^ World.name location;
      "Routes: " ^ routes;
      "Resources here: " ^ resources;
      "Inventory: " ^ inventory;
      "Crafted items: " ^ items;
      Printf.sprintf "Health: %d  Hunger: %d  Thirst: %d" (Player.health player)
        (Player.hunger player) (Player.thirst player);
      "Type help for commands.";
    ]

(** [render_success action] returns a message for a completed action. *)
let render_success = function
  | Game.Move location -> "Moved to " ^ World.name location ^ "."
  | Game.Gather resource -> "Gathered 1 " ^ Resource.name resource ^ "."
  | Game.Craft item -> "Crafted " ^ item_name item ^ "."
  | Game.Eat -> "Ate one berry."
  | Game.Drink -> "Drank one fresh water."
  | Game.Rest -> "Rested for 8 hours."

(** [render_error error] returns a readable reason for a failed action. *)
let render_error = function
  | Game.Move_error Player.No_route -> "There is no route to that location."
  | Game.Gather_error World.Unavailable_here ->
      "That resource is not available here."
  | Game.Gather_error World.Depleted -> "That resource has run out here."
  | Game.Consume_error (Player.Missing resource) ->
      "You need " ^ Resource.name resource ^ " in your inventory."
  | Game.Craft_error (Crafting.Missing_ingredients missing) ->
      "Missing ingredients: " ^ show_resources missing ^ "."
  | Game.Already_crafted item -> "You already crafted a " ^ item_name item ^ "."
  | Game.Game_over -> "The game is over."

(** [render_summary game] returns elapsed days and hours survived. *)
let render_summary game =
  let days, remaining_hours = days_and_hours (Game.elapsed_hours game) in
  let result = if Game.is_alive game then "Run ended." else "You died." in
  let day_word = if days = 1 then "day" else "days" in
  let hour_word = if remaining_hours = 1 then "hour" else "hours" in
  Printf.sprintf "%s Survived %d %s and %d %s." result days day_word
    remaining_hours hour_word
