type item = Pickaxe | Shelter

type error = Missing_ingredients of (Resource.t * int) list

let recipe = function
  | Pickaxe -> [ (Resource.Wood, 1); (Resource.Stone, 2) ]
  | Shelter -> [ (Resource.Wood, 3) ]

let craft stock item =
  let rec spend stock resource remaining =
    if remaining = 0 then Ok stock
    else
      match Resource.take_one stock resource with
      | Ok updated -> spend updated resource (remaining - 1)
      | Error Resource.Out_of_stock -> Error (resource, remaining)
  in
  let rec apply stock missing = function
    | [] ->
        if missing = [] then Ok (stock, item)
        else Error (Missing_ingredients (List.rev missing))
    | (resource, amount) :: rest -> (
        match spend stock resource amount with
        | Ok updated -> apply updated missing rest
        | Error shortage -> apply stock (shortage :: missing) rest)
  in
  apply stock [] (recipe item)
