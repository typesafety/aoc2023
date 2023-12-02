{-# OPTIONS_GHC -w #-}

-- | Module for re-exporting library modules qualified.  Intended to be loaded
-- in GHCi with `:load app/GHCI.hs`
module GHCI where

import MyPrelude

import Solutions.Day01 qualified as S01
import Solutions.Day01 qualified as S02
