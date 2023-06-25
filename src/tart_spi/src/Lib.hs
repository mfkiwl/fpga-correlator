{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable,
             OverloadedStrings, TypeOperators #-}

------------------------------------------------------------------------------
-- |
-- Module      : Lib
-- Copyright   : (C) Tim Molteno     2017
--               (C) Max Scheel      2017
--               (C) Patrick Suggate 2017
-- License     : GPL3
--
-- Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
-- Stability   : Experimental
-- Portability : non-portable
--
--
-- This file is part of TART.
--
-- TART is free software: you can redistribute it and/or modify it under the
-- terms of the GNU Lesser Public License as published by the Free Software
-- Foundation, either version 3 of the License, or (at your option) any later
-- version.
--
-- TART is distributed in the hope that it will be useful, but WITHOUT ANY
-- WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE.  See the GNU Lesser Public License for more
-- details.
--
-- You should have received a copy of the GNU Lesser Public License along with
-- TART.  If not, see <http://www.gnu.org/licenses/>.
--
--
-- Description:
-- Generates synthetic visibilities data, to be compared with the outputs of
-- TART's DSP functional-unit.
--
-- NOTE:
--  + requires `text` and `turtle` to be installed:
--      > cabal install text turtle
--      > make pairs
--      > make permute
--
-- Changelog:
--  + 14/06/2017  --  initial file;
--  + 24/06/2023  --  refactored into a compiled library;
--
-- FIXME:
--
-- TODO:
--
------------------------------------------------------------------------------

module Lib
  (
    calc
  , visb
  , dump

  , mfsr32
  , tobits
  , incr
  , mask

  , readPermutationVector
  )
where

import           Data.Bits
import           Data.Bool
import           Data.List                    (intercalate, tails)
import           Data.Word
import           Text.Printf

import           Control.Arrow                ((&&&))
import           Control.Monad.ST             (runST)
import qualified Data.Vector.Storable         as Vec
import qualified Data.Vector.Storable.Mutable as Mut


-- * Functions to compute the visibilities.
------------------------------------------------------------------------------
visb :: [Bool] -> [Bool] -> [([Int],[Int])] -> [([Int],[Int])]
visb (b:bs) (_:js) ((r,i):vs) = (corr b bs r, corr b js i):visb bs js vs
visb     _      _          _  = []

corr :: Bool -> [Bool] -> [Int] -> [Int]
corr b bs cs = zipWith (bool id succ) (map (b ==) bs) cs

incr :: [Bool] -> [Bool]
incr        []  = [True]
incr (False:bs) = True :bs
incr (True :bs) = False:incr bs

------------------------------------------------------------------------------
-- | Calculate the visibilies by correlating for `n` samples.
calc :: Int -> [[Bool]] -> ([[Bool]], [Int])
calc n bz =
  let ms = mean $ take n $ tail bz :: [Int]

      vs :: [([Int], [Int])]
      vs  =
        let l = length $ head bz
            f = init . tails . flip replicate 0 :: Int -> [[Int]]
            d = id &&& id
        in  d <$> f l

      go :: [[Bool]] -> [([Int], [Int])] -> [[([Int], [Int])]]
      go (a:b:bs) vz =
        let vz' = visb b a vz
        in  vz':go (b:bs) vz'
      go       _   _ = error $ printf "Lib.calc.go: invalid input (%s)\n" (show bz)

      xs :: [([Int], [Int])]
      xs  = go bz vs!!pred n

      ws = concat $ uncurry zip <$> xs
      ys = concat $ foldr (\(x, y) zs -> [x, y]:zs) [] ws :: [Int]

  in  (drop n bz, ys ++ ms)


-- * Helper functions.
------------------------------------------------------------------------------
mask :: Int -> Int -> [Bool]
mask l m = replicate m True ++ replicate (l-m) False

mean :: [[Bool]] -> [Int]
mean bz = go bz $ replicate n 0
  where
    n   = length bz
    go (b:bs) cs = bs `go` zipWith (bool id succ) b cs
    go    []  cs = cs

dump :: [Int] -> IO ()
dump vs = let ts  = printf "%06x " <$> vs :: [String]
              go _ [] = []
              go n xs = take n xs:go n (drop n xs)
              ts' = map unlines $ go 8 $ ('\t':) . concat <$> go 12 ts :: [String]
          in  putStrLn $ intercalate "\n" ts'

------------------------------------------------------------------------------
-- | Shift-register to use as a pseudorandom signal-source.
mfsr32 :: Word32 -> Word32
mfsr32 w = let tap0 = w .&. 0x02
               tap1 = w `shiftR` 26 .&. 0x04
               taps = tap0 .|. tap1
           in  w `rotateL` 1 `xor` taps

tobits :: Word32 -> [Bool]
tobits w = let go :: Int -> Word32 -> [Bool]
               go 32 _ = []
               go  n x = (x .&. 1 == 1):go (n+1) (x `shiftR` 1)
           in  go  0 w

------------------------------------------------------------------------------
-- | Permutations for the vectors of visibilities.
readPermutationVector :: FilePath -> IO [Int]
readPermutationVector  = fmap (inversePermutationVector . map read . words) . readFile

inversePermutationVector :: [Int] -> [Int]
inversePermutationVector ps = runST do
  let n = length ps
  ar <- Mut.new n
  let go _    []  = pure ()
      go i (j:js) = Mut.write ar j i >> go (i+1) js
  go 0 ps
  Vec.toList <$> Vec.freeze ar
