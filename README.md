Very simple Game of Life in Haskell
============================================

A simple test of NCurses "pseudo-GUI" library. Runs Conway's Game of Life on a grid wrapped into a torus.

Usage: gol [d:x,y] n:name | s:[x,y/] <init-spec> [[+ [x,y/] <init-spec>]], where:

- d:x,y specifies the dimensions of the world torus, with x and y integers
- n:name specifies one of built-in initial configuations. Two exist now, "glider" and "p15".
- s: is followed by a short-hand specification of the initial configuation. The spec consists of one of more blocks separated by '+'.
    - Each block consists of lines of characters '.' and 'x' representing respectively dead and live points.
    - Each line ends with the character '/' (also the last line).
    - Each block may be prefixed by an offset specified by row and column offset separated by comma and followed by '/'.

Examples:

- __gol d:20,30 n:glider__ runs the pre-built glider configuration on a 20x30 world torus
- __gol d:40,40 s:..x/x.x/.xx/ + 15,20/.xx/x.x/..x/__ runs two gliders on 40x40 grid, one starting from upper right-hand corner, 
  the other (inverted) offset 15 rows down and 20 columns to the left.


NB. This programme has the ncurses-0.2.16 package embedded into the repository, with a minor modification (in the file cbits/mavericks-c2hs-workaround.h header) to 
enable building on OS X Sierra.

Licensed under GPL-3, see the LICENSE file.
