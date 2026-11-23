(** The survivor: location, inventory, and survival statistics.

    Every statistic is an integer in [0 .. max_stat]. For [hunger] and [thirst],
    [max_stat] means fully fed or fully hydrated and [0] means starving or
    dehydrated. When the player's health reaches [0] the player is dead.

    Functions in this module never advance time on their own. After an action
    succeeds, the game loop must call [spend] exactly once with that action, so
    that every action costs time in the same way. *)

type t
(** An immutable player state. *)

val max_stat : int
(** [max_stat] is the largest value of any statistic (100). *)

val create :
  ?health:int ->
  ?hunger:int ->
  ?thirst:int ->
  ?inventory:Resource.stock ->
  World.location ->
  t
(** [create location] returns a player at [location] with every statistic at
    [max_stat] and an empty inventory. The optional arguments override those
    defaults; statistics outside [0 .. max_stat] are clamped into that range. *)

val location : t -> World.location
(** [location player] is where [player] currently is. *)

val health : t -> int
(** [health player] is [player]'s health. *)

val hunger : t -> int
(** [hunger player] is how well fed [player] is; [0] means starving. *)

val thirst : t -> int
(** [thirst player] is how hydrated [player] is; [0] means dehydrated. *)

val inventory : t -> Resource.stock
(** [inventory player] is the resources [player] is carrying. *)

val is_alive : t -> bool
(** [is_alive player] is whether [player]'s health is above zero. *)

(** {1 Actions} *)

(** Actions that take time. *)
type action =
  | Move
  | Gather
  | Craft
  | Eat
  | Drink
  | Rest

val hours : action -> int
(** [hours action] is how many in-game hours [action] takes. *)

(** The reason the player cannot move. *)
type move_error = No_route

val move : World.location -> t -> (t, move_error) result
(** [move destination player] returns [player] at [destination] when a direct
    route connects the player's location to it, or [Error No_route] otherwise.
    Statistics do not change; call [spend Move] afterwards. *)

val receive : Resource.t -> t -> t
(** [receive resource player] returns [player] carrying one more unit of
    [resource], for example after [World.gather]. *)

val with_inventory : Resource.stock -> t -> t
(** [with_inventory stock player] returns [player] carrying exactly [stock], for
    example the remaining stock after [Crafting.craft]. *)

(** The reason the player cannot eat or drink. *)
type consume_error = Missing of Resource.t

val eat : t -> (t, consume_error) result
(** [eat player] uses one unit of Berries and raises hunger by 20, up to
    [max_stat]. Returns [Error (Missing Berries)] and leaves [player] unchanged
    if none are carried. *)

val drink : t -> (t, consume_error) result
(** [drink player] uses one unit of Fresh_water and raises thirst by 30, up to
    [max_stat]. Returns [Error (Missing Fresh_water)] and leaves [player]
    unchanged if none are carried. *)

(** {1 Passing time} *)

val pass_hours : ?resting:bool -> int -> t -> t
(** [pass_hours n player] advances [player] by [n] hours. Each hour, hunger
    drops by 4 and thirst by 6 (never below 0). Then health drops by 10 for each
    of hunger and thirst that is at 0. If [resting] is [true] (default [false])
    and neither hunger nor thirst is at 0, health instead recovers by 5 that
    hour, up to [max_stat]. Raises [Invalid_argument] if [n] is negative. *)

val spend : action -> t -> t
(** [spend action player] is [pass_hours (hours action) player], resting when
    [action] is [Rest]. *)
