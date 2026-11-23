open OUnit2
open Island_survival

let tests =
  "crafting"
  >::: [
         ( "pickaxe consumes wood and stone" >:: fun _ ->
           let stock =
             Resource.stock_of_list
               [ (Resource.Wood, 2); (Resource.Stone, 2); (Resource.Berries, 1) ]
           in
           match Crafting.craft stock Crafting.Pickaxe with
           | Error _ -> assert_failure "expected enough ingredients"
           | Ok (remaining, item) ->
               assert_equal Crafting.Pickaxe item;
               assert_equal 1 (Resource.quantity remaining Resource.Wood);
               assert_equal 0 (Resource.quantity remaining Resource.Stone);
               assert_equal 1 (Resource.quantity remaining Resource.Berries) );
         ( "shelter consumes exactly three wood" >:: fun _ ->
           let stock = Resource.stock_of_list [ (Resource.Wood, 3) ] in
           match Crafting.craft stock Crafting.Shelter with
           | Error _ -> assert_failure "expected enough wood"
           | Ok (remaining, item) ->
               assert_equal Crafting.Shelter item;
               assert_equal 0 (Resource.quantity remaining Resource.Wood) );
         ( "failed craft reports the shortage and preserves stock" >:: fun _ ->
           let stock =
             Resource.stock_of_list [ (Resource.Wood, 1); (Resource.Stone, 1) ]
           in
           assert_equal
             (Error (Crafting.Missing_ingredients [ (Resource.Stone, 1) ]))
             (Crafting.craft stock Crafting.Pickaxe);
           assert_equal 1 (Resource.quantity stock Resource.Wood);
           assert_equal 1 (Resource.quantity stock Resource.Stone) );
         ( "empty stock reports all shortages" >:: fun _ ->
           assert_equal
             (Error
                (Crafting.Missing_ingredients
                   [ (Resource.Wood, 1); (Resource.Stone, 2) ]))
             (Crafting.craft (Resource.stock_of_list []) Crafting.Pickaxe) );
       ]

let () = run_test_tt_main tests
