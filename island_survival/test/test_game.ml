open OUnit2
open Island_survival

(** [ok result] returns the successful value or fails the test. *)
let ok = function
  | Ok value -> value
  | Error _ -> assert_failure "expected a successful action"

(** [check_error expected result] checks that [result] contains [expected]. *)
let check_error expected = function
  | Error actual -> assert_equal expected actual
  | Ok _ -> assert_failure "expected a failed action"

let initial_tests =
  "initial state"
  >::: [
         ( "starts at the beach with full stats" >:: fun _ ->
           let game = Game.initial () in
           let player = Game.player game in
           assert_equal World.Beach (Player.location player);
           assert_equal 100 (Player.health player);
           assert_equal 100 (Player.hunger player);
           assert_equal 100 (Player.thirst player);
           assert_equal 0 (Game.elapsed_hours game);
           assert_equal [] (Game.items game);
           assert_equal 0
             (Resource.quantity (Player.inventory player) Resource.Stone);
           assert_equal 6
             (World.quantity (Game.world game) World.Beach Resource.Stone) );
       ]

let movement_tests =
  "movement"
  >::: [
         ( "moving costs two hours" >:: fun _ ->
           let game =
             ok (Game.step (Game.initial ()) (Game.Move World.Forest))
           in
           assert_equal World.Forest (Player.location (Game.player game));
           assert_equal 2 (Game.elapsed_hours game);
           assert_equal 92 (Player.hunger (Game.player game));
           assert_equal 88 (Player.thirst (Game.player game)) );
         ( "invalid move costs no time" >:: fun _ ->
           let game = Game.initial () in
           check_error (Game.Move_error Player.No_route)
             (Game.step game (Game.Move World.Beach));
           assert_equal World.Beach (Player.location (Game.player game));
           assert_equal 0 (Game.elapsed_hours game) );
       ]

let gathering_tests =
  "gathering"
  >::: [
         ( "gather moves one unit into inventory" >:: fun _ ->
           let original = Game.initial () in
           let game = ok (Game.step original (Game.Gather Resource.Stone)) in
           assert_equal 1
             (Resource.quantity
                (Player.inventory (Game.player game))
                Resource.Stone);
           assert_equal 5
             (World.quantity (Game.world game) World.Beach Resource.Stone);
           assert_equal 6
             (World.quantity (Game.world original) World.Beach Resource.Stone);
           assert_equal 1 (Game.elapsed_hours game) );
         ( "unavailable resource costs no time" >:: fun _ ->
           let game = Game.initial () in
           check_error (Game.Gather_error World.Unavailable_here)
             (Game.step game (Game.Gather Resource.Wood));
           assert_equal 0 (Game.elapsed_hours game);
           assert_equal 6
             (World.quantity (Game.world game) World.Beach Resource.Stone) );
         ( "depleted resource costs no extra time" >:: fun _ ->
           let rec gather n game =
             if n = 0 then game
             else
               gather (n - 1) (ok (Game.step game (Game.Gather Resource.Stone)))
           in
           let game = gather 6 (Game.initial ()) in
           check_error (Game.Gather_error World.Depleted)
             (Game.step game (Game.Gather Resource.Stone));
           assert_equal 6 (Game.elapsed_hours game);
           assert_equal 0
             (World.quantity (Game.world game) World.Beach Resource.Stone) );
       ]

let survival_tests =
  "survival"
  >::: [
         ( "move gather and eat uses time once per action" >:: fun _ ->
           let game =
             ok (Game.step (Game.initial ()) (Game.Move World.Forest))
           in
           let game = ok (Game.step game (Game.Gather Resource.Berries)) in
           let game = ok (Game.step game Game.Eat) in
           assert_equal 4 (Game.elapsed_hours game);
           assert_equal 96 (Player.hunger (Game.player game));
           assert_equal 76 (Player.thirst (Game.player game));
           assert_equal 0
             (Resource.quantity
                (Player.inventory (Game.player game))
                Resource.Berries);
           assert_equal 5
             (World.quantity (Game.world game) World.Forest Resource.Berries) );
         ( "missing food and water cost no time" >:: fun _ ->
           let game = Game.initial () in
           check_error (Game.Consume_error (Player.Missing Resource.Berries))
             (Game.step game Game.Eat);
           check_error
             (Game.Consume_error (Player.Missing Resource.Fresh_water))
             (Game.step game Game.Drink);
           assert_equal 0 (Game.elapsed_hours game) );
         ( "drink uses water and spends one hour" >:: fun _ ->
           let game =
             ok (Game.step (Game.initial ()) (Game.Gather Resource.Fresh_water))
           in
           let game = ok (Game.step game Game.Drink) in
           assert_equal 2 (Game.elapsed_hours game);
           assert_equal 94 (Player.thirst (Game.player game));
           assert_equal 0
             (Resource.quantity
                (Player.inventory (Game.player game))
                Resource.Fresh_water) );
         ( "rest advances time" >:: fun _ ->
           let player = Player.create ~health:50 World.Beach in
           let game =
             ok (Game.step (Game.create (World.starter ()) player) Game.Rest)
           in
           assert_equal 8 (Game.elapsed_hours game);
           assert_equal 90 (Player.health (Game.player game)) );
         ( "death ends the run" >:: fun _ ->
           let player =
             Player.create ~health:10 ~hunger:0 ~thirst:0 World.Beach
           in
           let game =
             ok (Game.step (Game.create (World.starter ()) player) Game.Rest)
           in
           assert_bool "player is dead" (not (Game.is_alive game));
           assert_equal 8 (Game.elapsed_hours game);
           check_error Game.Game_over (Game.step game Game.Rest) );
       ]

let crafting_tests =
  "crafting in game"
  >::: [
         ( "pickaxe consumes ingredients and costs two hours" >:: fun _ ->
           let inventory =
             Resource.stock_of_list [ (Resource.Wood, 1); (Resource.Stone, 2) ]
           in
           let player = Player.create ~inventory World.Beach in
           let game =
             ok
               (Game.step
                  (Game.create (World.starter ()) player)
                  (Game.Craft Crafting.Pickaxe))
           in
           assert_equal [ Crafting.Pickaxe ] (Game.items game);
           assert_equal 2 (Game.elapsed_hours game);
           assert_equal 0
             (Resource.quantity
                (Player.inventory (Game.player game))
                Resource.Wood);
           assert_equal 0
             (Resource.quantity
                (Player.inventory (Game.player game))
                Resource.Stone) );
         ( "missing ingredients preserve inventory and time" >:: fun _ ->
           let game = Game.initial () in
           check_error
             (Game.Craft_error
                (Crafting.Missing_ingredients
                   [ (Resource.Wood, 1); (Resource.Stone, 2) ]))
             (Game.step game (Game.Craft Crafting.Pickaxe));
           assert_equal 0 (Game.elapsed_hours game);
           assert_equal 0
             (Resource.quantity
                (Player.inventory (Game.player game))
                Resource.Wood) );
         ( "duplicate craft preserves item and time" >:: fun _ ->
           let inventory = Resource.stock_of_list [ (Resource.Wood, 6) ] in
           let player = Player.create ~inventory World.Forest in
           let game =
             ok
               (Game.step
                  (Game.create (World.starter ()) player)
                  (Game.Craft Crafting.Shelter))
           in
           check_error (Game.Already_crafted Crafting.Shelter)
             (Game.step game (Game.Craft Crafting.Shelter));
           assert_equal [ Crafting.Shelter ] (Game.items game);
           assert_equal 2 (Game.elapsed_hours game);
           assert_equal 3
             (Resource.quantity
                (Player.inventory (Game.player game))
                Resource.Wood) );
       ]

let () =
  run_test_tt_main
    ("game"
    >::: [
           initial_tests;
           movement_tests;
           gathering_tests;
           survival_tests;
           crafting_tests;
         ])
