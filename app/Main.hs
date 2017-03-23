{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Main where

import UI.NCurses as NC

main :: IO ()
main = runCurses (showgrid initial)

tick :: ((Int, Int) -> Int) -> ((Int, Int) -> Int)
tick grid (x,y)
  | ( sqsum == 3 ) = 1
  | ( sqsum == 4) = grid (x,y)
  | otherwise = 0
    where sqsum = sum $ map grid [ (x+dx,y+dy) | dx <- [-1..1], dy <- [-1..1] ]

initial :: (Int,Int) -> Int
initial (x,y) =
  if (x == 2 && (y==1 || y == 2 || y == 3)) then 1 else 0

p :: ((Int,Int) -> Int) -> IO [()]
p f = let r = [ [f (x,y) | x <- [1..4] ] | y <- [1..4] ] in
  mapM (putStrLn.show) r

mark :: ((Int, Int) -> Int) -> (Int, Int) -> Update ()
mark f (x,y) = let c = [" ","X"]!! (f (x,y)) in
  do moveCursor (fromIntegral x) (fromIntegral y)
     drawString c

grid :: ((Int, Int) -> Int) -> Update [()]
grid f = sequence $ map (mark f) [ (x,y) | x <- [1..20],  y <- [1..20] ]

showgrid :: ((Int, Int) -> Int) -> Curses ()
showgrid f =
  do
  setEcho False
  w <- defaultWindow
  updateWindow w (grid f)
  render
  waitFor w (\ev -> (ev == EventCharacter 'q',f))

waitFor :: Window -> (Event -> (Bool,(Int,Int)->Int)) -> Curses ()
waitFor w p = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev' -> if fst (p ev') then showgrid (tick (snd (p ev'))) else loop
