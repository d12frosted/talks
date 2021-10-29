{-# LANGUAGE RankNTypes #-}

module Lens5 where

import Types

--------------------------------------------------------------------------------

newtype I a = MkI a

unI :: I a -> a
unI (MkI a) = a

instance Functor I where
  fmap f a = MkI (f (unI a))

--------------------------------------------------------------------------------

newtype C b a = MkC b

unC :: C b a -> b
unC (MkC b) = b

instance forall b. Functor (C b) where
  fmap _ (MkC b) = MkC b

--------------------------------------------------------------------------------

type Lens a b = forall t. Functor t => (b -> t b) -> (a -> t a)

set :: Lens a b -> b -> a -> a
set l b = over l (const b)

setF :: Functor f => Lens a b -> f b -> a -> f a
setF l b = l (const b)

over :: Lens a b -> (b -> b) -> (a -> a)
over l f a = unI $ l (MkI . f) a

view :: Lens a b -> a -> b
view l a = unC $ l MkC a

mkLens :: (a -> b) -> (b -> a -> a) -> Lens a b
mkLens v s f a = (`s` a) <$> f (v a)

comp :: Lens a b -> Lens b c -> Lens a c
comp l1 l2 = l1 . l2

--------------------------------------------------------------------------------

point :: Lens Atom Point
point = mkLens _point setPoint

element :: Lens Atom String
element = mkLens _element setElement

x :: Lens Point Double
x = mkLens _x setX

y :: Lens Point Double
y = mkLens _y setY

--------------------------------------------------------------------------------

setAtomX :: Double -> Atom -> Atom
setAtomX = set (point `comp` x)

--------------------------------------------------------------------------------

moveAtom :: Atom -> Atom
moveAtom = over (point `comp` x) (+ 1)

--------------------------------------------------------------------------------
