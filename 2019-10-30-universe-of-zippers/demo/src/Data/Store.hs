--------------------------------------------------------------------------------

{-# LANGUAGE NoImplicitPrelude #-}

--------------------------------------------------------------------------------

module Data.Store where

--------------------------------------------------------------------------------

import           Control.Comonad
import           RIO

--------------------------------------------------------------------------------

data Store s a = Store (s -> a) s

-- we could just derive it
instance Functor (Store s) where
  fmap f (Store g s) = Store (f . g) s

instance Comonad (Store s) where
  extract (Store f s) = f s
  duplicate (Store f s) = Store (Store f) s

--------------------------------------------------------------------------------
