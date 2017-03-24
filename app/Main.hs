{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import UI.NCurses as NC
import Data.Array.IArray

main :: IO ()
main = runCurses (showgrid initial)

type Position = (Integer, Integer)
type Grid = Array Position Integer

gridFromOnes :: (Position, Position) -> [Position] -> Grid
gridFromOnes size ones = array size $ map check (range size) where
  check pos = if pos `elem` ones then (pos,1) else (pos,0)

initial :: Grid
initial = gridFromOnes ((0,0),(20,20)) glider

fun :: [(Integer, Integer)]
fun = 
  [(10,5), (11,5), (12,5), (11,6), (11,7), (10,8), (11,8), (12,8),
   (10,10), (11,10), (12,10), (10,11), (11,11), (12,11),
   (10,13), (11,13), (12,13), (11,14), (11,15), (10,16), (11,16), (12,16)]

glider :: [(Integer, Integer)]
glider = [(5,5),(3,6),(5,6),(4,7),(5,7)]


tick :: Grid -> Position -> Integer
tick grid (x,y)
  | ( sqsum == 3 ) = 1
  | ( sqsum == 4) = grid ! (x,y)
  | otherwise = 0
    where sqsum = sum $ map (grid !) nbhd
          nbhd = [ wrap (x+dx, y+dy) | dx <- [-1..1], dy <- [-1..1] ]
          wrap (x,y) = (x `mod` (maxX + 1), y `mod` (maxY +1))
          (maxX,maxY) = snd $ bounds grid

next :: Grid -> Grid
next grid =
  array (bounds grid) $ map update (assocs grid)
  where update (pos,_) = (pos, tick grid pos)

mark :: Grid -> Position -> Update ()
mark grid (x,y) = 
  do moveCursor x y
     drawGlyph g
  where
    g = glyphs !! (fromInteger (grid ! (x,y)))
    glyphs = [Glyph ' ' [], glyphBlock]

draw :: Grid -> Update ()
draw grid = sequence_ $ map (mark grid) (indices grid)

instructions :: (Ix a, IArray a1 e) => a1 (a, Integer) e -> Update ()
instructions g = do
  moveCursor (snd(snd(bounds g))+5) 10
  drawString "Space to advance, q to exit."

showgrid :: Grid -> Curses ()
showgrid g =
  do
  setEcho False
  w <- defaultWindow
  updateWindow w (instructions g)
  updateWindow w (draw g)
  render
  waitFor w handle where
    handle (EventCharacter ' ') = Next g
    handle (EventCharacter 'q') = Quit
    handle _ = NOP

data Action = Next Grid | Quit | NOP

waitFor :: Window -> (Event -> Action) -> Curses ()
waitFor win handle = loop where
    loop = do
        ev <- getEvent win Nothing
        case ev of
            Nothing -> loop
            Just e -> case handle e of
              Next g -> showgrid (next g)
              Quit   -> return ()
              NOP    -> loop
