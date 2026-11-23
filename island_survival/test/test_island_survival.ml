open OUnit2
open Island_survival

let name_tests =
  "resource names"
  >::: [
         ( "stone" >:: fun _ ->
           assert_equal "Stone" (Resource.name Resource.Stone) );
         ( "fresh water" >:: fun _ ->
           assert_equal "Fresh water" (Resource.name Resource.Fresh_water) );
         ("wood" >:: fun _ -> assert_equal "Wood" (Resource.name Resource.Wood));
         ( "berries" >:: fun _ ->
           assert_equal "Berries" (Resource.name Resource.Berries) );
       ]

let stock_tests =
  "resource stocks"
  >::: [
         ( "take one leaves original unchanged" >:: fun _ ->
           let original =
             Resource.stock_of_list [ (Resource.Wood, 2); (Resource.Stone, 1) ]
           in
           match Resource.take_one original Resource.Wood with
           | Error _ -> assert_failure "expected to gather wood"
           | Ok updated ->
               assert_equal 2 (Resource.quantity original Resource.Wood);
               assert_equal 1 (Resource.quantity updated Resource.Wood);
               assert_equal 1 (Resource.quantity updated Resource.Stone) );
         ( "empty stock cannot be taken" >:: fun _ ->
           let stock = Resource.stock_of_list [ (Resource.Wood, 0) ] in
           assert_equal (Error Resource.Out_of_stock)
             (Resource.take_one stock Resource.Wood);
           assert_equal 0 (Resource.quantity stock Resource.Wood) );
         ( "absent resource has zero quantity" >:: fun _ ->
           let stock = Resource.stock_of_list [ (Resource.Wood, 2) ] in
           assert_equal 0 (Resource.quantity stock Resource.Berries);
           assert_equal (Error Resource.Out_of_stock)
             (Resource.take_one stock Resource.Berries) );
         ( "invalid quantities are rejected" >:: fun _ ->
           assert_raises
             (Invalid_argument "Resource.stock_of_list: negative quantity")
             (fun () ->
               ignore (Resource.stock_of_list [ (Resource.Stone, -1) ]));
           assert_raises
             (Invalid_argument "Resource.stock_of_list: duplicate resource")
             (fun () ->
               ignore
                 (Resource.stock_of_list
                    [ (Resource.Stone, 1); (Resource.Stone, 2) ])) );
       ]

let world_route_tests =
  "world routes"
  >::: [
         ( "beach connects to forest" >:: fun _ ->
           assert_equal [ World.Forest ] (World.destinations World.Beach);
           assert_bool "route exists" (World.can_move World.Beach World.Forest)
         );
         ( "forest connects to beach" >:: fun _ ->
           assert_equal [ World.Beach ] (World.destinations World.Forest);
           assert_bool "route exists" (World.can_move World.Forest World.Beach)
         );
         ( "staying put is not a route" >:: fun _ ->
           assert_bool "beach has no self route"
             (not (World.can_move World.Beach World.Beach));
           assert_bool "forest has no self route"
             (not (World.can_move World.Forest World.Forest)) );
         ( "location names" >:: fun _ ->
           assert_equal "Beach" (World.name World.Beach);
           assert_equal "Forest" (World.name World.Forest) );
       ]

let world_stock_tests =
  "world stocks"
  >::: [
         ( "beach starts with stone and fresh water" >:: fun _ ->
           let world = World.starter () in
           assert_equal
             [ (Resource.Stone, 6); (Resource.Fresh_water, 5) ]
             (World.resources world World.Beach);
           assert_equal 6 (World.quantity world World.Beach Resource.Stone);
           assert_equal 5
             (World.quantity world World.Beach Resource.Fresh_water) );
         ( "forest starts with wood and berries" >:: fun _ ->
           let world = World.starter () in
           assert_equal
             [ (Resource.Wood, 8); (Resource.Berries, 6) ]
             (World.resources world World.Forest);
           assert_equal 8 (World.quantity world World.Forest Resource.Wood);
           assert_equal 6 (World.quantity world World.Forest Resource.Berries)
         );
         ( "unavailable resources have zero quantity" >:: fun _ ->
           let world = World.starter () in
           assert_equal 0 (World.quantity world World.Beach Resource.Wood);
           assert_equal 0 (World.quantity world World.Forest Resource.Stone) );
       ]

let world_gather_tests =
  "world gathering"
  >::: [
         ( "gathering updates only the requested stock" >:: fun _ ->
           let original = World.starter () in
           match World.gather original World.Beach Resource.Stone with
           | Error _ -> assert_failure "expected stone at the beach"
           | Ok (updated, gathered) ->
               assert_equal Resource.Stone gathered;
               assert_equal 6
                 (World.quantity original World.Beach Resource.Stone);
               assert_equal 5
                 (World.quantity updated World.Beach Resource.Stone);
               assert_equal 5
                 (World.quantity updated World.Beach Resource.Fresh_water);
               assert_equal 8
                 (World.quantity updated World.Forest Resource.Wood) );
         ( "forest gathering returns berries" >:: fun _ ->
           let world = World.starter () in
           match World.gather world World.Forest Resource.Berries with
           | Error _ -> assert_failure "expected berries in the forest"
           | Ok (updated, gathered) ->
               assert_equal Resource.Berries gathered;
               assert_equal 5
                 (World.quantity updated World.Forest Resource.Berries) );
         ( "resource unavailable at location" >:: fun _ ->
           let world = World.starter () in
           assert_equal (Error World.Unavailable_here)
             (World.gather world World.Beach Resource.Wood);
           assert_equal 6 (World.quantity world World.Beach Resource.Stone) );
         ( "stock can be depleted" >:: fun _ ->
           let original = World.starter () in
           let rec gather_stone remaining world =
             if remaining = 0 then world
             else
               match World.gather world World.Beach Resource.Stone with
               | Ok (updated, Resource.Stone) ->
                   gather_stone (remaining - 1) updated
               | _ -> assert_failure "stone ran out too early"
           in
           let depleted = gather_stone 6 original in
           assert_equal 0 (World.quantity depleted World.Beach Resource.Stone);
           assert_equal (Error World.Depleted)
             (World.gather depleted World.Beach Resource.Stone);
           assert_equal 6 (World.quantity original World.Beach Resource.Stone);
           assert_equal
             [ (Resource.Stone, 0); (Resource.Fresh_water, 5) ]
             (World.resources depleted World.Beach) );
         ( "starter worlds are independent" >:: fun _ ->
           let first = World.starter () in
           let second = World.starter () in
           match World.gather first World.Forest Resource.Wood with
           | Error _ -> assert_failure "expected wood in the forest"
           | Ok (updated, _) ->
               assert_equal 7
                 (World.quantity updated World.Forest Resource.Wood);
               assert_equal 8 (World.quantity first World.Forest Resource.Wood);
               assert_equal 8 (World.quantity second World.Forest Resource.Wood)
         );
       ]

let () =
  run_test_tt_main
    ("island survival"
    >::: [
           name_tests;
           stock_tests;
           world_route_tests;
           world_stock_tests;
           world_gather_tests;
         ])
