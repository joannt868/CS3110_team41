open OUnit2
open Island_survival

(* Expected values below were worked out by hand from the rates documented in
   player.mli: hunger -4/hour, thirst -6/hour, -10 health/hour for each of
   hunger and thirst at 0, +5 health/hour when resting and neither is at 0. *)

let stock = Resource.stock_of_list

let assert_stats ~health ~hunger ~thirst p =
  assert_equal ~printer:string_of_int ~msg:"health" health (Player.health p);
  assert_equal ~printer:string_of_int ~msg:"hunger" hunger (Player.hunger p);
  assert_equal ~printer:string_of_int ~msg:"thirst" thirst (Player.thirst p)

let ok = function
  | Ok x -> x
  | Error _ -> assert_failure "expected Ok"

let creation_tests =
  "creation"
  >::: [
         ( "new player is full and empty-handed" >:: fun _ ->
           let p = Player.create World.Beach in
           assert_stats ~health:100 ~hunger:100 ~thirst:100 p;
           assert_equal World.Beach (Player.location p);
           assert_equal 0 (Resource.quantity (Player.inventory p) Resource.Wood);
           assert_bool "alive" (Player.is_alive p) );
         ( "out-of-range statistics are clamped" >:: fun _ ->
           let p =
             Player.create ~health:150 ~hunger:(-5) ~thirst:40 World.Beach
           in
           assert_stats ~health:100 ~hunger:0 ~thirst:40 p );
         ( "zero health is dead" >:: fun _ ->
           assert_bool "dead"
             (not (Player.is_alive (Player.create ~health:0 World.Beach))) );
       ]

let time_tests =
  "passing time"
  >::: [
         ( "one hour lowers hunger and thirst" >:: fun _ ->
           Player.create World.Beach |> Player.pass_hours 1
           |> assert_stats ~health:100 ~hunger:96 ~thirst:94 );
         ( "zero hours changes nothing" >:: fun _ ->
           Player.create ~hunger:50 World.Beach
           |> Player.pass_hours 0
           |> assert_stats ~health:100 ~hunger:50 ~thirst:100 );
         ( "negative hours are rejected" >:: fun _ ->
           assert_raises (Invalid_argument "Player.pass_hours: negative hours")
             (fun () -> Player.pass_hours (-1) (Player.create World.Beach)) );
         ( "thirst runs out first, then health drops" >:: fun _ ->
           (* thirst: 100 - 6*16 = 4 after 16 hours, 0 at hour 17. *)
           let p = Player.create World.Beach in
           Player.pass_hours 16 p
           |> assert_stats ~health:100 ~hunger:36 ~thirst:4;
           Player.pass_hours 17 p
           |> assert_stats ~health:90 ~hunger:32 ~thirst:0;
           Player.pass_hours 18 p
           |> assert_stats ~health:80 ~hunger:28 ~thirst:0 );
         ( "starving and dehydrated loses 20 health per hour" >:: fun _ ->
           Player.create ~hunger:0 ~thirst:0 World.Beach
           |> Player.pass_hours 1
           |> assert_stats ~health:80 ~hunger:0 ~thirst:0 );
         ( "health never goes below zero" >:: fun _ ->
           let p =
             Player.create ~health:15 ~hunger:0 ~thirst:0 World.Beach
             |> Player.pass_hours 1
           in
           assert_equal 0 (Player.health p);
           assert_bool "dead" (not (Player.is_alive p)) );
         ( "time without resting does not heal" >:: fun _ ->
           Player.create ~health:50 World.Beach
           |> Player.pass_hours 8
           |> assert_stats ~health:50 ~hunger:68 ~thirst:52 );
       ]

let action_tests =
  "action costs"
  >::: [
         ( "action hours" >:: fun _ ->
           assert_equal [ 2; 1; 2; 1; 1; 8 ]
             (List.map Player.hours
                Player.[ Move; Gather; Craft; Eat; Drink; Rest ]) );
         ( "spend Move costs two hours" >:: fun _ ->
           Player.create World.Beach |> Player.spend Player.Move
           |> assert_stats ~health:100 ~hunger:92 ~thirst:88 );
         ( "resting recovers 5 health per hour" >:: fun _ ->
           Player.create ~health:50 World.Beach
           |> Player.spend Player.Rest
           |> assert_stats ~health:90 ~hunger:68 ~thirst:52 );
         ( "resting cannot exceed max health" >:: fun _ ->
           Player.create ~health:98 World.Beach
           |> Player.spend Player.Rest
           |> assert_stats ~health:100 ~hunger:68 ~thirst:52 );
         ( "resting while dehydrated still loses health" >:: fun _ ->
           Player.create ~thirst:0 World.Beach
           |> Player.spend Player.Rest
           |> assert_stats ~health:20 ~hunger:68 ~thirst:0 );
       ]

let move_tests =
  "moving"
  >::: [
         ( "move along a route keeps statistics" >:: fun _ ->
           let p = Player.create ~hunger:70 World.Beach in
           let moved = ok (Player.move World.Forest p) in
           assert_equal World.Forest (Player.location moved);
           assert_stats ~health:100 ~hunger:70 ~thirst:100 moved;
           assert_equal World.Beach (Player.location p) );
         ( "move without a route fails" >:: fun _ ->
           assert_equal (Error Player.No_route)
             (Player.move World.Beach (Player.create World.Beach)) );
       ]

let inventory_tests =
  "inventory"
  >::: [
         ( "receive adds one unit each time" >:: fun _ ->
           let p =
             Player.create World.Forest
             |> Player.receive Resource.Wood
             |> Player.receive Resource.Wood
           in
           assert_equal 2 (Resource.quantity (Player.inventory p) Resource.Wood)
         );
         ( "with_inventory replaces the inventory" >:: fun _ ->
           let p =
             Player.create
               ~inventory:(stock [ (Resource.Wood, 3) ])
               World.Forest
             |> Player.with_inventory (stock [ (Resource.Stone, 1) ])
           in
           assert_equal 0 (Resource.quantity (Player.inventory p) Resource.Wood);
           assert_equal 1
             (Resource.quantity (Player.inventory p) Resource.Stone) );
       ]

let consume_tests =
  "eating and drinking"
  >::: [
         ( "eating uses a berry and adds 20 hunger" >:: fun _ ->
           let p =
             ok
               (Player.eat
                  (Player.create ~hunger:50
                     ~inventory:(stock [ (Resource.Berries, 2) ])
                     World.Forest))
           in
           assert_stats ~health:100 ~hunger:70 ~thirst:100 p;
           assert_equal 1
             (Resource.quantity (Player.inventory p) Resource.Berries) );
         ( "eating cannot exceed max hunger" >:: fun _ ->
           Player.create ~hunger:90
             ~inventory:(stock [ (Resource.Berries, 1) ])
             World.Forest
           |> Player.eat |> ok
           |> assert_stats ~health:100 ~hunger:100 ~thirst:100 );
         ( "eating with no berries fails and changes nothing" >:: fun _ ->
           let p = Player.create ~hunger:50 World.Forest in
           assert_equal (Error (Player.Missing Resource.Berries)) (Player.eat p);
           assert_stats ~health:100 ~hunger:50 ~thirst:100 p );
         ( "drinking uses water and adds 30 thirst" >:: fun _ ->
           let p =
             ok
               (Player.drink
                  (Player.create ~thirst:40
                     ~inventory:(stock [ (Resource.Fresh_water, 1) ])
                     World.Beach))
           in
           assert_stats ~health:100 ~hunger:100 ~thirst:70 p;
           assert_equal 0
             (Resource.quantity (Player.inventory p) Resource.Fresh_water) );
         ( "drinking with no water fails" >:: fun _ ->
           assert_equal (Error (Player.Missing Resource.Fresh_water))
             (Player.drink (Player.create World.Beach)) );
       ]

(* Uses World, Resource, and Player together the way the game loop will: move to
   the forest, gather berries, and eat them, spending time after each successful
   action. *)
let integration_tests =
  "world and player together"
  >::: [
         ( "move, gather, eat" >:: fun _ ->
           let world = World.starter () in
           let p = Player.create World.Beach in
           let p =
             ok (Player.move World.Forest p) |> Player.spend Player.Move
           in
           assert_stats ~health:100 ~hunger:92 ~thirst:88 p;
           let world, berry =
             ok (World.gather world (Player.location p) Resource.Berries)
           in
           let p = Player.receive berry p |> Player.spend Player.Gather in
           assert_stats ~health:100 ~hunger:88 ~thirst:82 p;
           let p = ok (Player.eat p) |> Player.spend Player.Eat in
           (* 88 + 20 = 108, clamped to 100, then -4 for the hour. *)
           assert_stats ~health:100 ~hunger:96 ~thirst:76 p;
           assert_equal 0
             (Resource.quantity (Player.inventory p) Resource.Berries);
           assert_equal 5 (World.quantity world World.Forest Resource.Berries)
         );
         ( "gathering where a resource is unavailable leaves the player alone"
         >:: fun _ ->
           let p = Player.create World.Beach in
           assert_equal (Error World.Unavailable_here)
             (World.gather (World.starter ()) (Player.location p) Resource.Wood);
           assert_stats ~health:100 ~hunger:100 ~thirst:100 p );
       ]

let () =
  run_test_tt_main
    ("player"
    >::: [
           creation_tests;
           time_tests;
           action_tests;
           move_tests;
           inventory_tests;
           consume_tests;
           integration_tests;
         ])
