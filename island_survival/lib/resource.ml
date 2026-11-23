type t =
  | Stone
  | Fresh_water
  | Wood
  | Berries

(** [name resource] returns the name displayed for [resource]. *)
let name = function
  | Stone -> "Stone"
  | Fresh_water -> "Fresh water"
  | Wood -> "Wood"
  | Berries -> "Berries"

type stock = (t * int) list
type take_error = Out_of_stock

(** [stock_of_list entries] returns a stock from unique resources with
    nonnegative quantities. *)
let stock_of_list entries =
  let rec validate seen = function
    | [] -> entries
    | (resource, count) :: rest ->
        if count < 0 then
          invalid_arg "Resource.stock_of_list: negative quantity"
        else if List.mem resource seen then
          invalid_arg "Resource.stock_of_list: duplicate resource"
        else validate (resource :: seen) rest
  in
  validate [] entries

(** [quantity stock resource] returns the quantity of [resource] in [stock]. *)
let quantity stock resource =
  match List.assoc_opt resource stock with
  | Some count -> count
  | None -> 0

(** [take_one stock resource] returns a new stock after removing one unit, or
    [Error Out_of_stock] when no units remain. The input stock is unchanged. *)
let take_one stock resource =
  if quantity stock resource = 0 then Error Out_of_stock
  else
    Ok
      (List.map
         (fun (kind, count) ->
           if kind = resource then (kind, count - 1) else (kind, count))
         stock)
