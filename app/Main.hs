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

import System.Environment
import UI.NCurses
import Data.Array.IArray
import Torus -- :: Array Position Integer;   Position :: (Integer,Integer)
import CLP (parseCLP)

-- This is where the Game of Life is specified:
-- An Integer array represents the world, which is wrapped up into a torus.
-- This function gets the next-round value of a cell on the "world" torus.
-- 1 is alive, 0 is dead.
tick :: Torus -> Position -> Integer
tick torus (x,y)
  | ( sqsum == 3 ) = 1
  | ( sqsum == 4) = torus ! (x,y)
  | otherwise = 0
    where sqsum = sum $ map (torus !) nbhd
          nbhd = [ pr (x+dx, y+dy) | dx <- [-1..1], dy <- [-1..1] ]
          pr = project (dims torus) -- we project the nbhd of point to torus

-- Produces the next iteration of the torus
next :: Torus -> Torus
next torus =
  array (bounds torus) $ map update (assocs torus)
  where update (pos,_) = (pos, tick torus pos)

--
-- Main program loop and graphical UI
--
main :: IO ()
main = do
  args <- getArgs
  let parsed = parseCLP $ concat args -- NB!! This kills white space
  case parsed of
    Right initial-> runCurses $ do
                        showInstructionsBelow initial
                        runTorus initial
    Left _         -> putStrLn "There was some issue with the command line parameters."

-- Helper to show positioned instruction text
showInstructionsBelow :: Torus -> Curses ()
showInstructionsBelow torus = do
  window <- defaultWindow
  updateWindow window $ do
     moveCursor (snd(dims torus)+5) 10
     drawString "Space to advance, q to exit."
  render

-- Main UI loop to show and update the torus
runTorus :: Torus -> Curses ()
runTorus t = do
  window <- defaultWindow
  setEcho False
  updateWindow window (draw t)
  render
  handle `eventsFor` window where
    handle (EventCharacter ' ') = runTorus (next t) 
    handle (EventCharacter 'q') = return ()
    handle _ = runTorus t 

-- Update whole torus to UI
draw :: Torus -> Update ()
draw t = sequence_ $ map (markPosFrom t) (indices t)

-- Update specified position from torus to UI
markPosFrom :: Torus -> Position -> Update ()
markPosFrom t (x,y) =
  do moveCursor x y
     drawGlyph g
  where
    g = glyphs !! (fromInteger (t ! (x,y)))
    glyphs = [Glyph ' ' [], glyphBlock]

-- UI even loop: runs specified event handler for a window
eventsFor ::(Event -> Curses ()) -> Window -> Curses ()
eventsFor handle win = loop where
    loop = do
        ev <- getEvent win Nothing
        case ev of
            Nothing -> loop
            Just e ->  handle e 
