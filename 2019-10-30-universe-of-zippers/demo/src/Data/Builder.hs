--------------------------------------------------------------------------------

{-# LANGUAGE NoImplicitPrelude #-}

--------------------------------------------------------------------------------

module Data.Builder where

--------------------------------------------------------------------------------

import           Control.Comonad
import           RIO

--------------------------------------------------------------------------------

type Option = String
newtype Config = MkConfig [Option] deriving (Show)

mkConfig :: [Option] -> Config
mkConfig = MkConfig

defaultConfig :: [Option] -> Config
defaultConfig options = MkConfig ("-Wall" : options)

profile :: ([Option] -> Config) -> Config
profile builder = builder ["-prof", "-auto-all"]

fullOpt :: ([Option] -> Config) -> Config
fullOpt builder = builder ["-O2"]

profile'  :: ([Option] -> Config) -> ([Option] -> Config)
profile' builder =
    \options -> builder (["-prof", "-auto-all"] ++ options)

fullOpt' :: ([Option] -> Config) -> ([Option] -> Config)
fullOpt' builder =
    \options -> builder (["-O2"] ++ options)

myConfig :: Config
myConfig
  = extract
  . extend fullOpt
  . extend profile
  $ defaultConfig

myConfig' :: Config
myConfig'
  = defaultConfig  -- new Builder()
  =>> profile      --   .profile()
  =>> fullOpt      --   .fullOpt()
  # extract        --   .extract

infixl 0 #
(#) :: a -> (a -> b) -> b
(#) = flip ($)

--------------------------------------------------------------------------------
