
-- The Torus type that holds the state of the Game of Life universe.
-- The universe is grid is wrapped around the edges to form a torus.

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


module Torus where

import Data.Array.IArray

type Position = (Integer, Integer)
type Dims = (Integer,Integer)
type Offset = (Integer,Integer)

-- Torus is represented by a rectangular grid stored in Array
-- The array represents a "fundamental domain" for the torus
type Torus = Array Position Integer

dims :: Torus -> Dims
dims = snd.bounds

-- Project position (from Z x Z) to the (fundamental domain of) torus
-- of dimensions
project :: Dims -> Position -> Position
project dims (x,y) = (x `mod` (mx+1), y `mod` (my +1)) where
  (mx,my) = dims

-- Build Torus of specified dimensions from a list of
-- specified "live" points (to be recorded as value 1 in the array)
fromOnes :: Dims -> [Position] -> Torus
fromOnes dims ones = array bounds $ map markPos (range bounds) where
  bounds = ((0,0), dims)
  markPos p = if p `elem` livepts then (p, 1) else (p, 0)
  livepts = map (project dims) ones
