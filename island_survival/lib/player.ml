type t = {
  location : World.location;
  health : int;
  hunger : int;
  thirst : int;
  inventory : Resource.stock;
}

let max_stat = 100
let hunger_per_hour = 4
let thirst_per_hour = 6
let damage_per_empty_stat = 10
let rest_recovery = 5
let food_value = 20
let water_value = 30

(** [clamp n] limits [n] to [0 .. max_stat]. *)
let clamp n = max 0 (min max_stat n)

let create ?(health = max_stat) ?(hunger = max_stat) ?(thirst = max_stat)
    ?(inventory = Resource.stock_of_list []) location =
  {
    location;
    health = clamp health;
    hunger = clamp hunger;
    thirst = clamp thirst;
    inventory;
  }

let location p = p.location
let health p = p.health
let hunger p = p.hunger
let thirst p = p.thirst
let inventory p = p.inventory
let is_alive p = p.health > 0

type action =
  | Move
  | Gather
  | Craft
  | Eat
  | Drink
  | Rest

let hours = function
  | Move -> 2
  | Gather -> 1
  | Craft -> 2
  | Eat -> 1
  | Drink -> 1
  | Rest -> 8

type move_error = No_route

let move destination p =
  if World.can_move p.location destination then
    Ok { p with location = destination }
  else Error No_route

let receive resource p =
  { p with inventory = Resource.add p.inventory resource 1 }

let with_inventory inventory p = { p with inventory }

type consume_error = Missing of Resource.t

(** [consume resource update p] uses one [resource] from [p]'s inventory and
    applies [update] to the result, or reports that [resource] is missing. *)
let consume resource update p =
  match Resource.take_one p.inventory resource with
  | Error Resource.Out_of_stock -> Error (Missing resource)
  | Ok inventory -> Ok (update { p with inventory })

let eat =
  consume Resource.Berries (fun p ->
      { p with hunger = clamp (p.hunger + food_value) })

let drink =
  consume Resource.Fresh_water (fun p ->
      { p with thirst = clamp (p.thirst + water_value) })

(** [one_hour ~resting p] advances [p] by a single hour. *)
let one_hour ~resting p =
  let hunger = clamp (p.hunger - hunger_per_hour) in
  let thirst = clamp (p.thirst - thirst_per_hour) in
  let damage =
    (if hunger = 0 then damage_per_empty_stat else 0)
    + if thirst = 0 then damage_per_empty_stat else 0
  in
  let recovery = if resting && damage = 0 then rest_recovery else 0 in
  { p with hunger; thirst; health = clamp (p.health - damage + recovery) }

let pass_hours ?(resting = false) n p =
  if n < 0 then invalid_arg "Player.pass_hours: negative hours"
  else
    let rec loop n p =
      if n = 0 then p else loop (n - 1) (one_hour ~resting p)
    in
    loop n p

let spend action p = pass_hours ~resting:(action = Rest) (hours action) p
