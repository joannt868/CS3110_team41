open OUnit2
open Island_survival

let tests =
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

let () = run_test_tt_main tests
