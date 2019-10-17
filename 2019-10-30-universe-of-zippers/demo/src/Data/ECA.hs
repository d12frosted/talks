--------------------------------------------------------------------------------

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Data.ECA where

--------------------------------------------------------------------------------

import           Data.Display.Extra
import           Data.Universe

--------------------------------------------------------------------------------

import           Control.Comonad
import           RIO

--------------------------------------------------------------------------------

data Cell
  = Dead
  | Alive

instance Display Cell where
  display Dead  = "□"
  display Alive = "■"

--------------------------------------------------------------------------------

rule110 :: U Cell -> Cell
rule110 u = case extract3 Dead u of
  (Dead  , Dead  , Dead)  -> Dead
  (Dead  , Dead  , Alive) -> Alive
  (Dead  , Alive , Dead)  -> Alive
  (Dead  , Alive , Alive) -> Alive
  (Alive , Dead  , Dead)  -> Dead
  (Alive , Dead  , Alive) -> Alive
  (Alive , Alive , Dead)  -> Alive
  (Alive , Alive , Alive) -> Dead

--------------------------------------------------------------------------------

extract3 :: Cell -> U Cell -> (Cell, Cell, Cell)
extract3 c u = case maybe c extract . ($ u) <$> [maybeLeft, Just, maybeRight] of
  [l, c, r] -> (l, c, r)

--------------------------------------------------------------------------------
