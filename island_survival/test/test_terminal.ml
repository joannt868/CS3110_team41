open OUnit2
open Island_survival

(** [line_present line text] returns whether [text] contains [line] as a full
    line. *)
let line_present line text = List.mem line (String.split_on_char '\n' text)

let parsing_tests =
  "terminal parsing"
  >::: [
         ( "commands ignore case and surrounding spaces" >:: fun _ ->
           assert_equal (Ok (Terminal.Action (Game.Move World.Forest)))
             (Terminal.parse "  MOVE   FoReSt  ");
           assert_equal (Ok Terminal.Status) (Terminal.parse " Status ");
           assert_equal (Ok Terminal.Help) (Terminal.parse "HELP");
           assert_equal (Ok Terminal.Quit) (Terminal.parse " quit ") );
         ( "water names and other actions" >:: fun _ ->
           assert_equal
             (Ok (Terminal.Action (Game.Gather Resource.Fresh_water)))
             (Terminal.parse "gather fresh water");
           assert_equal
             (Ok (Terminal.Action (Game.Gather Resource.Fresh_water)))
             (Terminal.parse "gather water");
           assert_equal (Ok (Terminal.Action (Game.Craft Crafting.Pickaxe)))
             (Terminal.parse "craft pickaxe");
           assert_equal (Ok (Terminal.Action Game.Eat)) (Terminal.parse "eat");
           assert_equal (Ok (Terminal.Action Game.Drink))
             (Terminal.parse "drink");
           assert_equal (Ok (Terminal.Action Game.Rest)) (Terminal.parse "rest")
         );
         ( "invalid commands return guidance" >:: fun _ ->
           assert_equal
             (Error "Enter a command. Type help to see your options.")
             (Terminal.parse "   ");
           assert_equal
             (Error
                "Unknown command or arguments. Type help to see your options.")
             (Terminal.parse "move mountain") );
       ]

let display_tests =
  "terminal display"
  >::: [
         ( "initial frame shows relevant game state" >:: fun _ ->
           let frame = Terminal.render_state (Game.initial ()) in
           assert_bool "day and hour" (line_present "Day 1, Hour 0" frame);
           assert_bool "location" (line_present "Location: Beach" frame);
           assert_bool "routes" (line_present "Routes: Forest" frame);
           assert_bool "resources"
             (line_present "Resources here: Stone: 6, Fresh water: 5" frame);
           assert_bool "inventory"
             (line_present
                "Inventory: Stone: 0, Fresh water: 0, Wood: 0, Berries: 0" frame);
           assert_bool "items" (line_present "Crafted items: none" frame);
           assert_bool "statistics"
             (line_present "Health: 100  Hunger: 100  Thirst: 100" frame) );
         ( "clock wraps after 24 hours" >:: fun _ ->
           let game = Game.initial () in
           let game =
             match Game.step game Game.Rest with
             | Ok updated -> updated
             | Error _ -> assert_failure "rest should succeed"
           in
           let game =
             match Game.step game Game.Rest with
             | Ok updated -> updated
             | Error _ -> assert_failure "second rest should succeed"
           in
           let game =
             match Game.step game Game.Rest with
             | Ok updated -> updated
             | Error _ -> assert_failure "third rest should succeed"
           in
           assert_bool "next day"
             (line_present "Day 2, Hour 0" (Terminal.render_state game));
           assert_equal "Run ended. Survived 1 day and 0 hours."
             (Terminal.render_summary game) );
         ( "crafted item appears in the status frame" >:: fun _ ->
           let inventory = Resource.stock_of_list [ (Resource.Wood, 3) ] in
           let player = Player.create ~inventory World.Beach in
           let game = Game.create (World.starter ()) player in
           let game =
             match Game.step game (Game.Craft Crafting.Shelter) with
             | Ok updated -> updated
             | Error _ -> assert_failure "shelter should be craftable"
           in
           assert_bool "shelter shown"
             (line_present "Crafted items: Shelter"
                (Terminal.render_state game)) );
         ( "errors explain shortages and missing resources" >:: fun _ ->
           assert_equal "That resource has run out here."
             (Terminal.render_error (Game.Gather_error World.Depleted));
           assert_equal "Missing ingredients: Wood: 1, Stone: 2."
             (Terminal.render_error
                (Game.Craft_error
                   (Crafting.Missing_ingredients
                      [ (Resource.Wood, 1); (Resource.Stone, 2) ]))) );
       ]

let () = run_test_tt_main ("terminal" >::: [ parsing_tests; display_tests ])
