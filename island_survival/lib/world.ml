type location =
  | Beach
  | Forest

type t = {
  beach : Resource.stock;
  forest : Resource.stock;
}

(** [name location] returns the name displayed for [location]. *)
let name = function
  | Beach -> "Beach"
  | Forest -> "Forest"

(** [destinations location] returns the locations directly reachable from
    [location]. *)
let destinations = function
  | Beach -> [ Forest ]
  | Forest -> [ Beach ]

(** [can_move origin destination] returns whether a direct route connects
    [origin] to [destination]. *)
let can_move origin destination = List.mem destination (destinations origin)

(** [starter ()] returns the fixed starter island and its initial stocks. *)
let starter () =
  {
    beach =
      Resource.stock_of_list [ (Resource.Stone, 6); (Resource.Fresh_water, 5) ];
    forest =
      Resource.stock_of_list [ (Resource.Wood, 8); (Resource.Berries, 6) ];
  }

(** [native_resources location] returns the resource kinds available at
    [location]. *)
let native_resources = function
  | Beach -> [ Resource.Stone; Resource.Fresh_water ]
  | Forest -> [ Resource.Wood; Resource.Berries ]

(** [stock_at world location] returns the stock at [location]. *)
let stock_at world = function
  | Beach -> world.beach
  | Forest -> world.forest

(** [resources world location] returns the native resources at [location] and
    their remaining quantities. *)
let resources world location =
  List.map
    (fun resource ->
      (resource, Resource.quantity (stock_at world location) resource))
    (native_resources location)

(** [quantity world location resource] returns the remaining quantity at
    [location]. *)
let quantity world location resource =
  Resource.quantity (stock_at world location) resource
