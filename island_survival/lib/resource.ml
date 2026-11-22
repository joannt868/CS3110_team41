type t =
  | Stone
  | Fresh_water
  | Wood
  | Berries

(** [name resource] is the name displayed for [resource]. *)
let name = function
  | Stone -> "Stone"
  | Fresh_water -> "Fresh water"
  | Wood -> "Wood"
  | Berries -> "Berries"
