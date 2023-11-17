# Instructions
User is assumed should have ocaml and opam installed; however, if the user does \
not, then follow installation instructions according to the [OCaml textbook](https://cs3110.github.io/textbook/chapters/preface/install.html).

To play the game, run `make play` in the terminal.

# Game Rules
Your mission is to make it out alive. You spawn in a dungeon in the midst of a \
battle and fighting your way through the floor is the goal of the game. Along \
the way, you'll encounter several crossroads, each leading to different events. \
You can skip battles all you want, but be careful! Each staircase to the next \
floor is guarded by a boss. You won't have the resources to defeat it if you \
spend a life on the run! In battle, you will take a turn to  `play [card]` from \
your and and move it to your active slots. After using up your energy, you `end` \
your turn and wait to see the damage that has been dealt. You will eventually \
choose a door with `go [door number]`. The chance event will perhaps provide \
you with some luck against your enemies. Shop encounter will give you the \
opportunity to buy new cards if you are dealt a bad hand. Camp encounter will\
give you the energy and health you need to keep on fighting. At any time, feel \
free to `quit` the game and start anew. Should you perish along the way, you'll \
have the option to wake up at the beginning of the floor.

When you are confused, use the `help` command to see what commands are avaible \
to you. Good luck!