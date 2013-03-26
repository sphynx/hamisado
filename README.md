## Summary

Hamisado is an AI system to play or analyze Kamisado games written in
Haskell.

The origin of the name is quite obvious: I've changed the first letter
of Kamisado to honor Haskell :)

Kamisado is a quick abstract game which I've become a fan of. To learn
the rules and to know more, please follow the link to [BGG][]

## Kamisado game description

A quck preview is here (it may sound a little bit advertising, I
believe this comes from the game publishers):

> "Kamisado is a game of pure skill and strategy! There are no dice,
> cards or any other chance element. It’s just you against your
> opponent! The aim in each round is to be the first to get an octagonal
> ‘dragon tower’ to the opposite side of the board, by moving the towers
> in straight lines, either forwards or diagonally forwards. It sounds
> easy doesn’t it, but the twist is that you can only move a tower if
> its colour matches the colour of the square that your opponent last
> moved to. Also, you will find that the routes you want to use are
> blocked by enemy towers (and sometimes your own!). As the game
> unfolds, your towers will be promoted to ‘Sumos’, and will have the
> ability to push your opponent's pieces backwards, earning you extra
> turns. The situations continue to become more complex and challenging,
> until one player accumulates the required winning total and can be
> declared a ‘Kamisado Grand Master’ - until the next game!"

Please note that my implementation is unofficial and does not relate
to the game publishers in any way.

## Installation and usage

You'll need a Haskell platform installed to be able to compile from
source. It can be downloaded and installed from [Haskell Platform][]

Then, when you have it, you can just type this in the project
directory:

    cabal install

This should build "Hamisado" executable.

Here is a summary of the options available:

    sphynx@fjord% ./Hamisado --help

    Hamisado [OPTIONS]

    Common flags:
      -a --algorithm=ALGORITHM            alphabeta | negascout | minimax |
                                          idalpha
      -i --implementation=IMPLEMENTATION  gametree | my | tzaar
      -d --depth=INT                      Depth
      -p --playmode=PLAYMODE              losing | best | play | perft | solve
      -t --turn=AIPLAYER                  first | second | analyse
      -? --help                           Display help message
      -V --version                        Print version information

You may parametrize AI over algorithms and implementation to
experiment with performance. Defaults work the best now
(algo=negascout, impl=my).

In order to play the game with AI moves first try this:

    Hamisado -p play -t first -d 10

Moves are entered and displayed in the algebraic notation, e.g. a1-a7
(the first rank is near the player who moves first).

There are five running modes (`-p` parameter): "play" for playing the
game, "best" for searching for the best move in the starting position,
"losing" for searching the starting moves of the first player which
lose by force after they are done, "perft" for performing the move
generator testing (it reports a number of the game tree leaves on
given depth) and "solve" for solving the game (i.e. proving that the
game is won by the first player from the starting position).

Feel free to experiment.

Game play example:

    sphynx@fjord% ./Hamisado -p play -t second -d 10
    Params: mode=Play, algo=Negascout, implementation=My, depth=10
    Enter your turn: f1-f7
    Score: -1. PV: [E8-E3,C1-C7,D8-D4,A1-A3,B8-B3,D1-E2,G8-G2,H1-H7,C8-H3,B1-G6]
    AI move: E8-E3
    Enter your turn: c1-c6
    Score: -1. PV: [A8-A5,E1-D2,B8-G3,D2-D6,F8-H6,G1-H2,H6-F4,H2-H7,C8-B7,H1-E4]
    AI move: A8-A5
    Enter your turn: ^C


## Some implementation details

This code may be considered as an exercise in abstract games
programming and AI techniques. Plus lots of fun from mixing together
two quite pleasant things: Haskell and board games :)

I've implemented the alpha-beta pruning and negascout algorithms to
prune the game tree significantly. Sorting moves using a simple but
effective heuristics also turned to be crucial for the performance.

This code also includes alpha-beta and negascout implementations from
HsTzaar (an AI to play Tzaar game) by Pedro Vasconcelos in order to
compare the performance between the implementations. Also I've tried
to use game-tree package from Hackage. It's possible to switch between
the implementations.

The program is console-based, but I must admit that it's a not a very
good idea for a board game implementation, therefore I'm going to add
GUI later.

[Haskell Platform]: http://www.haskell.org/platform/
[BGG]: http://boardgamegeek.com/boardgame/38545/kamisado
