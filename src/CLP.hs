-- Command line parsing and processing for Game of Life app

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


-- TO DO
--------------------------------------------------------------------
-- IMPLEMENT: file parsing
-- IMPLEMENT: proper failure for named" parser if name not found
-- IMPLEMENT: option h => show help
-- IMPLEMENT: Permutation parsing

module CLP where

import Text.Megaparsec
import Text.Megaparsec.String
import qualified Data.Map.Lazy as M (Map,lookup,fromList)
import Torus

parseCLP :: String -> Either (ParseError Char Dec) Torus
parseCLP str = runParser clp "" str

-- Parser for the full command line.
-- Returns the world torus with its initial state.
clp :: Parser Torus
clp = do
  d <- optional dim
  ones <-  named <|> clspec-- <|> file
  let d' = case d of
        Nothing -> (20,20) -- default dims of the world
        Just tuple -> tuple
  return $ fromOnes d' ones

-- Possible explicit world dimensions
dim ::  Parser (Integer, Integer)
dim = string "d:" >> npair

-- Parser for comma-separated pair of Integers
npair :: Parser Position
npair = do
  wds <- some digitChar
  char ','
  hds <- some digitChar
  space
  return $ (read wds, read hds)

-- Command-line specification in dot-x format
clspec:: Parser [Position]
clspec= string "s:" >> spec

-- Get filename and parse dot-x formatted spec in file
file :: Parser [Position]
file = undefined

fileName :: Parser String
fileName = do
  string "f:"
  many (alphaNumChar <|> oneOf ['.','/','~'])

-- ===========================================================
-- Parse the dot-x formatted specification language
-- Use '.' for zero, 'x' for one and '/' or <eol> for new row
-- Optional off-set at start with 'row,col/': 5,7/..x/x/
-- Finally, '+' separates individual fragments.
-- ===========================================================

pts = many (char '.')

x   = do
  char 'x'
  pos <- getPosition
  return $ ((toInteger.unPos.sourceColumn) pos)

nl  = string "/" <|> eol

one = try (pts >> x)

line :: Parser ([Integer],Integer)
line = do
  pos <- getPosition
  os <- many one
  pts
  nl
  let b = (toInteger.unPos.sourceColumn) pos
  return (os,b)

dotX = do
  ls <- some line
  let ls' = zip ls [1..]
  let ones = [ p | line <- ls', p <- adj line ] where
        adj :: (([Integer], Integer),Integer) -> [Position]
        adj ((lpos, start),ln) = map (\n -> (ln, n - start)) lpos
  return ones

offset :: Parser (Integer, Integer)
offset = do
  dp <- npair
  char '/'
  return dp

fragment = do
  o <- optional offset
  ones <- dotX
  let ones' = case o of
        Nothing -> ones
        Just (dx,dy) -> map (\(x,y) -> (x+dx,y+dy)) ones
  return ones'

-- Full specification of one or more (possibly off-set) fragments
spec :: Parser [Position]
spec = do
  f <- fragment
  fs <- many ((char '+') >> fragment)
  return  $ concat (f:fs)

-- ================================================================

-- Use one of the named pre-made configurations
named :: Parser [Position]
named = do
  string "n:"
  name <- many alphaNumChar
  let ones = M.lookup name namedArr
  case ones of
    Just x  -> return x
    Nothing -> undefined -- This will crash at runtime => fix (requires more general error handling!)

-- Named intial arrangements
namedArr :: M.Map String [Position]
namedArr = M.fromList [("p15", p15), ("glider", glider)]

-- 1. Arrangement with a period of 15
p15 :: [(Integer, Integer)]
p15 = 
  [(10,5), (11,5), (12,5), (11,6), (11,7), (10,8), (11,8), (12,8),
   (10,10), (11,10), (12,10), (10,11), (11,11), (12,11),
   (10,13), (11,13), (12,13), (11,14), (11,15), (10,16), (11,16), (12,16)]

-- 2. Diagonally moving "glider"
glider :: [(Integer, Integer)]
glider = [(5,5),(3,6),(5,6),(4,7),(5,7)]



-- ========================
-- Testing
-- ========================



-- TEST CODE TO REMOVE
test = parse spec ""
