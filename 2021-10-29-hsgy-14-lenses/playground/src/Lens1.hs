module Lens1 where

import Types

--------------------------------------------------------------------------------

data Lens a b = Lens
  { view :: a -> b,
    set :: b -> a -> a
  }

--------------------------------------------------------------------------------

comp' :: Lens a b -> Lens b c -> Lens a c
comp' l1 l2 = Lens (view l2 . view l1) (\c a -> set l1 (set l2 c (view l1 a)) a)

--------------------------------------------------------------------------------

point :: Lens Atom Point
point = Lens _point setPoint

element :: Lens Atom String
element = Lens _element setElement

x :: Lens Point Double
x = Lens _x setX

y :: Lens Point Double
y = Lens _y setY

--------------------------------------------------------------------------------

setAtomX :: Double -> Atom -> Atom
setAtomX = set (point `comp` x)

--------------------------------------------------------------------------------

over :: Lens a b -> (b -> b) -> (a -> a)
over l f a = set l (f (view l a)) a

--------------------------------------------------------------------------------

moveAtom :: Atom -> Atom
moveAtom = over (point `comp` x) (+ 1)

--------------------------------------------------------------------------------

comp :: Lens a b -> Lens b c -> Lens a c
comp l1 l2 = Lens (view l2 . view l1) (over l1 . set l2)

--------------------------------------------------------------------------------
