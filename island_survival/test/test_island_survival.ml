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

let () =
  run_test_tt_main
    ("island survival" >::: [ name_tests; stock_tests; world_route_tests ])
