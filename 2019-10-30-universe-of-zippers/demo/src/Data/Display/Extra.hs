--------------------------------------------------------------------------------

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}

--------------------------------------------------------------------------------

module Data.Display.Extra where

--------------------------------------------------------------------------------

import           Data.Text.IO (putStrLn)
import           RIO

--------------------------------------------------------------------------------

data DisplayStyle
  = DisplayTogether
  | DisplayFocused

class StyledDisplay a where
  stylish :: DisplayStyle -> a -> Utf8Builder

print :: (MonadIO m, StyledDisplay a) => a -> m ()
print = print' DisplayTogether

print' :: ( MonadIO m
          , StyledDisplay a
          ) => DisplayStyle -> a -> m ()
print' style = liftIO . putStrLn . textDisplay . stylish style

--------------------------------------------------------------------------------
