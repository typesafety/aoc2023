{-# HLINT ignore #-}
{-# OPTIONS_GHC -w #-}
{-# language NoImplicitPrelude #-}
{-# language OverloadedLabels #-}

-- | Module for re-exporting library modules qualified.  Intended to be loaded
-- in GHCi with `:load app/GHCI.hs`
module GHCI where

import MyPrelude

import Solutions.Day01 qualified as S01
import Solutions.Day02 qualified as S02
import Solutions.Day03 qualified as S03
import Solutions.Day04 qualified as S04
import Solutions.Day05 qualified as S05
import Solutions.Day06 qualified as S06
import Solutions.Day07 qualified as S07
import Solutions.Day08 qualified as S08
import Solutions.Day09 qualified as S09
import Solutions.Day10 qualified as S10
import Solutions.Day11 qualified as S11
import Solutions.Day12 qualified as S12
import Solutions.Day13 qualified as S13
import Solutions.Day14 qualified as S14
import Solutions.Day15 qualified as S15
import Solutions.Day16 qualified as S16
