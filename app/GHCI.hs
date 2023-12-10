{-# OPTIONS_GHC -w #-}
{-# language NoImplicitPrelude #-}

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
