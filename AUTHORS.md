# Source acknowledgments

## AI assistance

Erik Mauricio(em882) used Codex to help implement the Resource and World modules and draft their OUnit tests. I reviewed the code, checked the expected test values by hand against the rates documented in resource.mli and world.mli, and can explain how it works. This assistance covered:
- `island_survival/lib/resource.ml`, `island_survival/lib/resource.mli`
- `island_survival/lib/world.ml`, `island_survival/lib/world.mli`
- `island_survival/test/test_island_survival.ml`

Codex also assisted with the terminal game and command parser interfaces/implementations, display, and their OUnit tests. This assistance covered:
- `island_survival/lib/game.ml`, `island_survival/lib/game.mli`
- `island_survival/lib/terminal.ml`, `island_survival/lib/terminal.mli`
- `island_survival/bin/main.ml`, `island_survival/test/test_game.ml`, `island_survival/test/test_terminal.ml`
- `README.md` 

Trenton Banks (tib24) used Claude (Anthropic) to help draft the Player module,
Resource.add, and their OUnit tests. I reviewed the code, checked the expected
test values by hand against the rates documented in player.mli, and can explain
how it works. This assistance covered:
- `island_survival/lib/player.ml`, `island_survival/lib/player.mli`
- `island_survival/lib/resource.ml`, `island_survival/lib/resource.mli` (`add`)
- `island_survival/test/test_player.ml`, `island_survival/test/test_island_survival.ml` (`add` tests)

Joann Tran (jt868) used ChatGPT to help write the four OUnit tests. This assistance covered:
- `island_survival/test/test_crafting.ml`

