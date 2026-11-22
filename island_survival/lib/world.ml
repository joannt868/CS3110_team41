type location = Beach | Forest

(** [name location] returns the name displayed for [location]. *)
let name = function Beach -> "Beach" | Forest -> "Forest"

(** [destinations location] returns the locations directly reachable from
    [location]. *)
let destinations = function Beach -> [ Forest ] | Forest -> [ Beach ]

(** [can_move origin destination] returns whether a direct route connects
    [origin] to [destination]. *)
let can_move origin destination = List.mem destination (destinations origin)
