--------------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Data.Universe2
  ( U2(..)
  , fromList2
  , shift
  , maybeShift
  , narrow2
  , narrow2'
  , widen2
  , Direction(..)
  , allDirections
  ) where

--------------------------------------------------------------------------------

import           Data.Display.Extra
import           Data.Universe

--------------------------------------------------------------------------------

import           Control.Comonad
import           RIO
import           RIO.List           (findIndex, intersperse, iterate, repeat)
import           RIO.List.Partial   (tail)
import           RIO.Text           (unpack)

--------------------------------------------------------------------------------

newtype U2 a
  = U2
  { getUniverse :: U (U a)
  } deriving (Functor)

instance Comonad U2 where
  extract = extract . extract . getUniverse
  duplicate = fmap U2 . U2 . shifted . shifted . getUniverse
    where shifted :: U (U a) -> U (U (U a))
          shifted u@(U _ (U ls _ rs) _) = U
            (tail $ iterate (fmap left) u)
            u
            (tail $ iterate (fmap right) u)

--------------------------------------------------------------------------------

data StyledU2 a
  = StyledU2
  { styledUniverse   :: U2 a
  , styledFocusLeft  :: Utf8Builder
  , styledFocusRight :: Utf8Builder
  , styledSeparator  :: Utf8Builder
  }

instance Display a => StyledDisplay (U2 a) where
  stylish DisplayTogether u = display $ StyledU2
    { styledUniverse = u
    , styledFocusLeft = ""
    , styledFocusRight = ""
    , styledSeparator = ""
    }
  stylish DisplayFocused u = display $ StyledU2
    { styledUniverse = u
    , styledFocusLeft = "["
    , styledFocusRight = "]"
    , styledSeparator = " "
    }

instance Display a => Display (StyledU2 a) where
  display (StyledU2 (U2 (U ls m rs)) fl fr sep)
    = mconcat
    [ sep
    , mconcat $ intersperse ("\n" <> sep) (displayStrip fl fr <$> reverse ls)
    , "\n", fl
    , displayStrip sep sep m
    , fr, "\n", sep
    , mconcat $ intersperse ("\n" <> sep) (displayStrip fl fr <$> rs)
    ]
    where displayStrip lsep rsep (U ls' a rs')
            = mconcat
            [ mconcat (display <$> reverse ls')
            , lsep
            , display a
            , rsep
            , mconcat (display <$> rs')
            ]

--------------------------------------------------------------------------------

data Direction
  = North
  | East
  | South
  | West
  deriving (Eq, Ord, Enum, Bounded, Show)

instance Display Direction where
  display North = "N"
  display South = "S"
  display East  = "E"
  display West  = "W"

allDirections :: [Direction]
allDirections = [minBound..maxBound]

--------------------------------------------------------------------------------

shift :: Direction -> U2 a -> U2 a
shift North (U2 u) = U2 $ left      u
shift South (U2 u) = U2 $ right     u
shift East  (U2 u) = U2 $ right <$> u
shift West  (U2 u) = U2 $ left  <$> u

maybeShift :: Direction -> U2 a -> Maybe (U2 a)
maybeShift North (U2 u) = U2 <$> maybeLeft u
maybeShift South (U2 u) = U2 <$> maybeRight u
maybeShift East  (U2 u) = U2 <$> sequence (maybeRight <$> u)
maybeShift West  (U2 u) = U2 <$> sequence (maybeLeft  <$> u)

--------------------------------------------------------------------------------

narrow2' :: Int -> Int -> Int -> Int -> U2 a -> U2 a
narrow2' xl xr yl yr = U2 . fmap (narrow' xl xr) . narrow' yl yr . getUniverse

narrow2 :: Int -> U2 a -> U2 a
narrow2 n = narrow2' n n n n

widen2 :: a -> U2 a -> U2 a
widen2 a (U2 u) = U2 $ widen a <$> widen (repeatU a) u

--------------------------------------------------------------------------------

fromList2 :: a -> [[a]] -> U2 a
fromList2 d = U2 . fromList (repeatU d) . fmap (fromList d)

--------------------------------------------------------------------------------
