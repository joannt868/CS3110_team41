(** The playable game state and action rules. *)

type t
(** An immutable game state. *)

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

val create : World.t -> Player.t -> t
(** [create world player] returns a game with [world] and [player], no crafted
    items, and zero elapsed hours. It supports tests with an explicit starting
    situation. *)

val initial : unit -> t
(** [initial ()] returns a game starting at the Beach with full statistics and
    an empty inventory. *)

val world : t -> World.t
(** [world game] returns the current island. *)

val player : t -> Player.t
(** [player game] returns the current player. *)

val items : t -> Crafting.item list
(** [items game] returns the crafted items in their crafting order. *)

val elapsed_hours : t -> int
(** [elapsed_hours game] returns the number of in-game hours spent. *)

val is_alive : t -> bool
(** [is_alive game] returns whether the player is alive. *)

val step : t -> action -> (t, error) result
(** [step game action] returns the next state after a successful [action]. It
    spends time exactly once on success. On failure it returns an error and
    leaves [game] unchanged. Actions after death return [Error Game_over]. *)
