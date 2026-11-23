(** Tools and structures made from gathered resources. *)
type item = Pickaxe | Shelter

type error = Missing_ingredients of (Resource.t * int) list

val recipe : item -> (Resource.t * int) list
(** [recipe item] lists the ingredients required to make one [item]. *)

val craft :
  Resource.stock -> item -> (Resource.stock * item, error) result
(** [craft stock item] returns the remaining stock and crafted item on success.
    On failure it reports each missing resource and the amount still needed.
    The input stock is immutable. *)
