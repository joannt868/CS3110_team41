(** Resources available on the island. *)
type t =
  | Stone
  | Fresh_water
  | Wood
  | Berries

val name : t -> string
(** [name resource] returns the name displayed for [resource]. *)

type stock
(** An immutable collection of resource quantities. *)

(** The reason a resource cannot be taken from a stock. *)
type take_error = Out_of_stock

val stock_of_list : (t * int) list -> stock
(** [stock_of_list entries] returns a stock from [entries]. Each resource may
    occur at most once and each quantity must be nonnegative; otherwise it
    raises [Invalid_argument]. *)

val quantity : stock -> t -> int
(** [quantity stock resource] returns the number of units of [resource] in
    [stock], or zero if it is absent. *)

val take_one : stock -> t -> (stock, take_error) result
(** [take_one stock resource] returns a new stock with one fewer unit of
    [resource]. It returns [Error Out_of_stock] if none remain. The original
    stock is unchanged. *)
