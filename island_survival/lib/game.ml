type t = {
  world : World.t;
  player : Player.t;
  items : Crafting.item list;
  elapsed_hours : int;
}

type action =
  | Move of World.location
  | Gather of Resource.t
  | Craft of Crafting.item
  | Eat
  | Drink
  | Rest

type error =
  | Move_error of Player.move_error
  | Gather_error of World.gather_error
  | Consume_error of Player.consume_error
  | Craft_error of Crafting.error
  | Already_crafted of Crafting.item
  | Game_over

(** [create world player] returns a game with no crafted items or elapsed time.
*)
let create world player = { world; player; items = []; elapsed_hours = 0 }

(** [initial ()] returns the fixed starter world and a full player at the Beach.
*)
let initial () = create (World.starter ()) (Player.create World.Beach)

(** [world game] returns the current island. *)
let world game = game.world

(** [player game] returns the current player. *)
let player game = game.player

(** [items game] returns the crafted items in their crafting order. *)
let items game = game.items

(** [elapsed_hours game] returns the number of in-game hours spent. *)
let elapsed_hours game = game.elapsed_hours

(** [is_alive game] returns whether the player is alive. *)
let is_alive game = Player.is_alive game.player

(** [complete game action world player items] returns a state after spending
    time once for a successful [action]. *)
let complete game action world player items =
  {
    world;
    player = Player.spend action player;
    items;
    elapsed_hours = game.elapsed_hours + Player.hours action;
  }

(** [apply_move game destination] returns the game after moving, or reports that
    no route exists. *)
let apply_move game destination =
  match Player.move destination game.player with
  | Error reason -> Error (Move_error reason)
  | Ok player -> Ok (complete game Player.Move game.world player game.items)

(** [apply_gather game resource] returns the game after collecting one
    [resource], or reports why gathering failed. *)
let apply_gather game resource =
  match World.gather game.world (Player.location game.player) resource with
  | Error reason -> Error (Gather_error reason)
  | Ok (world, gathered) ->
      let player = Player.receive gathered game.player in
      Ok (complete game Player.Gather world player game.items)

(** [apply_craft game item] returns the game after making [item], or reports
    missing ingredients or a duplicate item. *)
let apply_craft game item =
  if List.mem item game.items then Error (Already_crafted item)
  else
    match Crafting.craft (Player.inventory game.player) item with
    | Error reason -> Error (Craft_error reason)
    | Ok (stock, crafted) ->
        let player = Player.with_inventory stock game.player in
        Ok
          (complete game Player.Craft game.world player
             (game.items @ [ crafted ]))

(** [apply_consume game action consume] returns the game after [consume]
    succeeds and spends time for [action], or reports a missing resource. *)
let apply_consume game action consume =
  match consume game.player with
  | Error reason -> Error (Consume_error reason)
  | Ok player -> Ok (complete game action game.world player game.items)

(** [step game action] returns the updated state, or an error without changing
    [game]. *)
let step game action =
  if not (is_alive game) then Error Game_over
  else
    match action with
    | Move destination -> apply_move game destination
    | Gather resource -> apply_gather game resource
    | Craft item -> apply_craft game item
    | Eat -> apply_consume game Player.Eat Player.eat
    | Drink -> apply_consume game Player.Drink Player.drink
    | Rest -> Ok (complete game Player.Rest game.world game.player game.items)
