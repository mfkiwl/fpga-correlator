#!/usr/bin/env runhaskell
{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable,
             OverloadedStrings, TemplateHaskell, TypeOperators #-}

------------------------------------------------------------------------------
-- |
-- Module      : Main
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
--
-- FIXME:
--
-- TODO:
--
------------------------------------------------------------------------------

module Main where

import           Control.Arrow       (first)
import           Control.Lens        (makeLenses)
import           GHC.Generics        (Generic)
import           Options.Applicative
import           Text.Printf

import           Lib


-- * App data-types
------------------------------------------------------------------------------
-- | Command-line options.
data TartOpts
  = TartOpts
      { _numAntennae :: Int
      , _antennaMask :: Int
      , _counterBits :: Int
      , _dropSamples :: Int
      , _permuteFile :: FilePath
      }
  deriving (Eq, Generic, Show)

makeLenses ''TartOpts


-- * Configuration functions
------------------------------------------------------------------------------
-- | Command-line options parser.
parser :: Parser TartOpts
parser  = TartOpts
  <$> option auto (short 'l' <> long "antennas" <> metavar "NUM" <>
                   help "Number of antennas" <> value 24 <> showDefault)
  <*> option auto (short 'm' <> long "mask" <> metavar "BITS" <>
                   help "Bit-mask for each antennna/source" <>
                   value 0 <> showDefault)
  <*> option auto (short 'e' <> long "counter-bits" <> metavar "NUM" <>
                   help "Number of bits for the sample-counter" <>
                   value 12 <> showDefault)
  <*> option auto (short 'd' <> long "drop-samples" <> value 2 <> showDefault <>
                   help "Number of intial samples to drop")
  <*> strOption   (short 'p' <> long "permute" <> metavar "FILE" <>
                   help "File containing the permutation vector" <>
                   value "data/permute.txt" <> showDefault)


-- * Generate visibilities from synthetic inputs.
------------------------------------------------------------------------------
-- | Compute synthetic visibilities, and with input arguments:
--    `l`  --  number of antennae (or, length of bit-strings);
--    `m`  --  number of mask-bits, for testing subranges of the input;
--    `e`  --  exponent of the number of iterations; and
--    `d`  --  number of initial samples to drop.
--
--   For example, to duplicate the behaviour of `tart_dsp_tb`, `d = 2`, and
--   `l = 24`.
--
--   To run:
--
--     > stack run -- -l 24 -m 0 -e 12 -d 2 -p data/permute.txt
--
main :: IO ()
main  = do
  TartOpts l m e d fp <- execParser (info parser mempty)
  ps <- readPermutationVector fp --  "data/permute.txt"

  let bz = drop d $ map (take l . tobits) $ iterate mfsr32 1
      n  = 2^e
--       cz = drop d $ map (zipWith (&&) ms) $ iterate incr $ replicate l False
--       qz = drop d $ map (take l . tobits) $ enumFrom 0
--       ms = mask l m
--       (bz', xs) = calc n bz
--       (_  , ys) = calc n bz'
      (ys, xs) = first (snd . calc n) $ calc n bz

  printf "Synthetic visibilities data-generator (%d, %d, %d, %d):\n" l m n d

  printf "\n\nBank 0:\n"
  dump $ map (xs!!) ps

  printf "\n\nBank 1:\n"
  dump $ map (ys!!) ps
