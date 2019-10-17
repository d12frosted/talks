--------------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Data.Universe
  ( U(..)
  , fromList
  , repeatU
  , left
  , right
  , maybeLeft
  , maybeRight
  , narrow
  , narrow'
  , widen
  ) where

--------------------------------------------------------------------------------

import           Data.Display.Extra

--------------------------------------------------------------------------------

import           Control.Comonad
import           RIO
import           RIO.List           (foldl, intersperse, iterate, repeat)
import           RIO.List.Partial   (tail, (!!))

--------------------------------------------------------------------------------

data U a = U [a] a [a]
  deriving (Eq, Show, Functor)

--------------------------------------------------------------------------------

instance Comonad U where
  extract (U _ x _) = x
  duplicate u = U (tail $ iterate left u) u (tail $ iterate right u)

--------------------------------------------------------------------------------

instance Foldable U where
 foldr f z (U ls x rs) = foldl (flip f) (foldr f z (x:rs)) ls

instance Traversable U where
 traverse f (U ls x rs) = U <$>
    (reverse <$> traverse f (reverse ls)) <*> f x <*> traverse f rs

--------------------------------------------------------------------------------

repeatU :: a -> U a
repeatU a = U (repeat a) a (repeat a)

fromList :: a -> [a] -> U a
fromList d []     = repeatU d
fromList d (x:xs) = U (repeat d) x (xs ++ repeat d)

--------------------------------------------------------------------------------

left :: U a -> U a
left w = fromMaybe w $ maybeLeft w

right :: U a -> U a
right w = fromMaybe w $ maybeRight w

maybeLeft :: U a -> Maybe (U a)
maybeLeft (U []     _ _)  = Nothing
maybeLeft (U (l:ls) a rs) = Just $ U ls l (a:rs)

maybeRight :: U a -> Maybe (U a)
maybeRight (U _  _ [])     = Nothing
maybeRight (U ls a (r:rs)) = Just $ U (a:ls) r rs

--------------------------------------------------------------------------------

narrow :: Int -> U a -> U a
narrow r = narrow' r r

narrow' :: Int -> Int -> U a -> U a
narrow' l r (U ls a rs) = U (take l ls) a (take r rs)

widen :: a -> U a -> U a
widen c (U ls a rs) = U (ls <> repeat c) a (rs <> repeat c)

--------------------------------------------------------------------------------

data StyledU a
  = StyledU
  { styledUniverse   :: U a
  , styledFocusLeft  :: Utf8Builder
  , styledFocusRight :: Utf8Builder
  , styledListLeft   :: Utf8Builder
  , styledListRight  :: Utf8Builder
  , styledSeparator  :: Utf8Builder
  }

instance Display a => StyledDisplay (U a) where
  stylish DisplayTogether u = display $ StyledU
    { styledUniverse   = u
    , styledFocusLeft  = ""
    , styledFocusRight = ""
    , styledListLeft   = ""
    , styledListRight  = ""
    , styledSeparator  = ""
    }
  stylish DisplayFocused u = display $ StyledU
    { styledUniverse   = u
    , styledFocusLeft  = " ("
    , styledFocusRight = ") "
    , styledListLeft   = "["
    , styledListRight  = "]"
    , styledSeparator  = ", "
    }

instance Display a => Display (StyledU a) where
  display (StyledU (U l c r) fLeft fRight lLeft lRight sep)
    = mconcat
    [ lLeft
    , mconcat . intersperse sep . fmap display . reverse $ l
    , lRight
    , fLeft
    , display c
    , fRight
    , lLeft
    , mconcat . intersperse sep . fmap display $ r
    , lRight
    ]

--------------------------------------------------------------------------------
