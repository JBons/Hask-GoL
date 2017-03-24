Very simple Game of Life in Haskell
============================================

A simple test of NCurses "pseudo-GUI" library. Runs Conway's Game of Life on a (20x20) grid wrapped into a torus.
No CLI arguments, selection of initial arrangement by editing Main.hs (contains two samples).

NB. This programme has the ncurses-0.2.16 package embedded into the repository, with a minor modification (in the file cbits/mavericks-c2hs-workaround.h header) to 
enable building on OS X Sierra.

Licensed under GPL-3, see the LICENSE file.
