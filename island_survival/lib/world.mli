(** Locations on the starter island. *)
type location =
  | Beach
  | Forest

val name : location -> string
(** [name location] returns the name displayed for [location]. *)

val destinations : location -> location list
(** [destinations location] returns the locations directly reachable from
    [location]. *)

val can_move : location -> location -> bool
(** [can_move origin destination] returns whether a direct route connects
    [origin] to [destination]. *)
