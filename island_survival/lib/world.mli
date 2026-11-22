(** Locations on the starter island. *)
type location =
  | Beach
  | Forest

type t
(** The island and its remaining resources. *)

(** The reason gathering cannot succeed. *)
type gather_error =
  | Unavailable_here
  | Depleted

val name : location -> string
(** [name location] returns the name displayed for [location]. *)

val destinations : location -> location list
(** [destinations location] returns the locations directly reachable from
    [location]. *)

val can_move : location -> location -> bool
(** [can_move origin destination] returns whether a direct route connects
    [origin] to [destination]. *)

val starter : unit -> t
(** [starter ()] returns the fixed starter island. The Beach has six Stone and
    five Fresh_water; the Forest has eight Wood and six Berries. *)

val resources : t -> location -> (Resource.t * int) list
(** [resources world location] returns the native resources at [location] and
    their remaining quantities, including resources whose quantity is zero. *)

val quantity : t -> location -> Resource.t -> int
(** [quantity world location resource] returns the remaining quantity of
    [resource] at [location], or zero if it is unavailable there. *)

val gather :
  t -> location -> Resource.t -> (t * Resource.t, gather_error) result
(** [gather world location resource] returns an updated world and one unit of
    [resource] when that resource is available at [location]. It returns
    [Error Unavailable_here] if the resource does not occur there, or
    [Error Depleted] if its stock is empty. The input world is unchanged. *)
