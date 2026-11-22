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

let () = run_test_tt_main ("resources" >::: [ name_tests; stock_tests ])
