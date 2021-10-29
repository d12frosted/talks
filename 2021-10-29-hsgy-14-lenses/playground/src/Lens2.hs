module Lens2 where

import Types

--------------------------------------------------------------------------------

data Lens a b = Lens
  { view :: a -> b,
    set :: b -> a -> a,
    over :: (b -> b) -> (a -> a)
  }

mkLens :: (a -> b) -> (b -> a -> a) -> Lens a b
mkLens v s = Lens v s (\f a -> s (f (v a)) a)

--------------------------------------------------------------------------------

comp :: Lens a b -> Lens b c -> Lens a c
comp l1 l2 =
  Lens
    (view l2 . view l1)
    (over l1 . set l2)
    (over l1 . over l2)

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
