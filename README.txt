Hamisado is an AI system to play or analyze Kamisado games written in
Haskell.

The origin of the name is quite obvious: I've changed the first letter
of Kamisado to honor Haskell :)

Kamisado is a quite nice abstract game which I've become a fan of. To
learn the rules and to know more, please follow the link to BGG:

http://boardgamegeek.com/boardgame/38545/kamisado

A quck preview is here (it may sound a little bit advertising, I
believe this comes from the game publishers):

"Kamisado is a game of pure skill and strategy! There are no dice,
cards or any other chance element. It’s just you against your
opponent! The aim in each round is to be the first to get an octagonal
‘dragon tower’ to the opposite side of the board, by moving the towers
in straight lines, either forwards or diagonally forwards. It sounds
easy doesn’t it, but the twist is that you can only move a tower if
its colour matches the colour of the square that your opponent last
moved to. Also, you will find that the routes you want to use are
blocked by enemy towers (and sometimes your own!). As the game
unfolds, your towers will be promoted to ‘Sumos’, and will have the
ability to push your opponent's pieces backwards, earning you extra
turns. The situations continue to become more complex and challenging,
until one player accumulates the required winning total and can be
declared a ‘Kamisado Grand Master’ - until the next game!"

Please note that my implementation is unofficial and does not relate
to the game publishers in any way.

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
