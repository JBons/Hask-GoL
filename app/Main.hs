-- Copyright (C) 2017 Juhani Bonsdorf
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Main where

import UI.NCurses
import Data.Array.IArray

type Position = (Integer, Integer)
type Grid = Array Position Integer

-- Build grid from specified "live" positions (recorded as value 1)
gridFromOnes :: (Position, Position) -> [Position] -> Grid
gridFromOnes size ones = array size $ map check (range size) where
  check pos = if pos `elem` ones then (pos,1) else (pos,0)

-- Update of a cell in the grid.
-- The grid "wraps around the edges", forming torus
tick :: Grid -> Position -> Integer
tick grid (x,y)
  | ( sqsum == 3 ) = 1
  | ( sqsum == 4) = grid ! (x,y)
  | otherwise = 0
    where sqsum = sum $ map (grid !) nbhd
          nbhd = [ wrap (x+dx, y+dy) | dx <- [-1..1], dy <- [-1..1] ]
          wrap (x,y) = (x `mod` (maxX + 1), y `mod` (maxY +1))
          (maxX,maxY) = snd $ bounds grid

-- Produces the next iteration of the grid
next :: Grid -> Grid
next grid =
  array (bounds grid) $ map update (assocs grid)
  where update (pos,_) = (pos, tick grid pos)

--
-- Main program loop and graphical UI
--

-- No CLI arguments implemented yet, just run the automaton
main :: IO ()
main = runCurses $ do
  let start = gridFromOnes ((0,0),(20,20)) fun
  showInstructionsBelow start
  runGrid start

-- Helper to show positioned instruction text
showInstructionsBelow :: Grid -> Curses ()
showInstructionsBelow grid = do
  window <- defaultWindow
  updateWindow window $ do
     moveCursor (snd(snd(bounds grid))+5) 10
     drawString "Space to advance, q to exit."
  render

-- Main UI loop to show and update the grid
runGrid :: Grid -> Curses ()
runGrid grid = do
  window <- defaultWindow
  setEcho False
  updateWindow window (draw grid)
  render
  handle `eventsFor` window where
    handle (EventCharacter ' ') = runGrid (next grid) 
    handle (EventCharacter 'q') = return ()
    handle _ = runGrid grid 

-- Update whole grid to UI
draw :: Grid -> Update ()
draw grid = sequence_ $ map (markPosFrom grid) (indices grid)

-- Update specified position from grid to UI
markPosFrom :: Grid -> Position -> Update ()
markPosFrom grid (x,y) =
  do moveCursor x y
     drawGlyph g
  where
    g = glyphs !! (fromInteger (grid ! (x,y)))
    glyphs = [Glyph ' ' [], glyphBlock]

-- UI even loop: runs specified event handler for a window
eventsFor ::(Event -> Curses ()) -> Window -> Curses ()
eventsFor handle win = loop where
    loop = do
        ev <- getEvent win Nothing
        case ev of
            Nothing -> loop
            Just e ->  handle e 

-- Intial arrangements

-- 1. Arrangement with a period of 15
fun = 
  [(10,5), (11,5), (12,5), (11,6), (11,7), (10,8), (11,8), (12,8),
   (10,10), (11,10), (12,10), (10,11), (11,11), (12,11),
   (10,13), (11,13), (12,13), (11,14), (11,15), (10,16), (11,16), (12,16)]

-- 2. Diagonally moving "glider"
glider = [(5,5),(3,6),(5,6),(4,7),(5,7)]
