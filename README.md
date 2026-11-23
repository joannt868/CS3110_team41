# CS3110 Team 41

## Team Members

- Joann Tran (NetID: jt868)
- Erik Mauricio (NETID: em882)
- Trenton Banks (NETID: tib24)

## Run the game

From `island_survival/`, run:

```sh
dune build
dune exec bin/main.exe
```

Run the automated tests with `dune test`. The game starts at the Beach with full
health, hunger, and thirst, and an empty inventory. Each turn shows the current
day and hour, location, routes, local resources, inventory, crafted items, and
survival statistics. The goal is to survive as many in-game days as possible.

## Commands

Commands ignore capitalization and spaces around the command. Type `help` in
the game to see this list.

| Command | Result | Time |
| --- | --- | --- |
| `move beach` or `move forest` | Move along a direct route | 2 hours |
| `gather stone`, `gather water`, `gather wood`, or `gather berries` | Collect one unit at the current location | 1 hour |
| `craft pickaxe` or `craft shelter` | Use inventory resources to make one item | 2 hours |
| `eat` | Use one berry to restore hunger | 1 hour |
| `drink` | Use one fresh water to restore thirst | 1 hour |
| `rest` | Rest and recover health when fed and hydrated | 8 hours |
| `status` | Show the current game state | None |
| `help` | Show command instructions | None |
| `quit` | End the run and show survival time | None |

`gather fresh water` also works. Gathering is limited by the resources at each
location: the Beach begins with six stone and five fresh water; the Forest has
eight wood and six berries. A pickaxe costs one wood and two stone, and a shelter
costs three wood. You can make each item once. Crafted items are recorded and
displayed, but they do not change gameplay yet.

Successful actions reduce hunger and thirst as time passes. Health reaches zero
if survival needs are neglected, ending the run. Invalid commands and failed
actions do not spend time.
