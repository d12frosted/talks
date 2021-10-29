{-# LANGUAGE RankNTypes #-}

module Lens6 where

import Types

--------------------------------------------------------------------------------

newtype I a = MkI a

unI :: I a -> a
unI (MkI a) = a

instance Functor I where
  fmap f a = MkI (f (unI a))

instance Applicative I where
  pure a = MkI a
  f <*> a = MkI $ (unI f) (unI a)

--------------------------------------------------------------------------------

newtype C b a = MkC b

unC :: C b a -> b
unC (MkC b) = b

instance Functor (C b) where
  fmap _ (MkC b) = MkC b

instance Monoid b => Applicative (C b) where
  pure _ = MkC mempty
  f <*> a = MkC (unC f <> unC a)

--------------------------------------------------------------------------------

newtype CL b a = MkCL [b]

unCL :: CL b a -> [b]
unCL (MkCL bs) = bs

instance Functor (CL b) where
  fmap _ (MkCL bs) = MkCL bs

instance Applicative (CL b) where
  pure _ = MkCL []
  f <*> a = MkCL (unCL f <> unCL a)

--------------------------------------------------------------------------------

type Lens a b = forall t. Functor t => (b -> t b) -> (a -> t a)

setF :: Functor f => Lens a b -> f b -> a -> f a
setF l b = l (const b)

view :: Lens a b -> a -> b
view l a = unC $ l MkC a

mkLens :: (a -> b) -> (b -> a -> a) -> Lens a b
mkLens v s f a = (`s` a) <$> f (v a)

--------------------------------------------------------------------------------

type Traversal a b = forall t. Applicative t => (b -> t b) -> (a -> t a)

lensToTraversal ::
  (forall t. Functor t => (b -> t b) -> (a -> t a)) ->
  (forall t. Applicative t => (b -> t b) -> (a -> t a))
lensToTraversal l = l

-- traversalToLens ::
--   (forall t. Applicative t => (b -> t b) -> (a -> t a)) ->
--   (forall t. Functor t => (b -> t b) -> (a -> t a))
-- traversalToLens l = l

over :: Traversal a b -> (b -> b) -> (a -> a)
over l f a = unI $ l (MkI . f) a

set :: Traversal a b -> b -> a -> a
set l b = over l (const b)

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
setAtomX = set (point . x)

--------------------------------------------------------------------------------

moveAtom :: Atom -> Atom
moveAtom = over (point . x) (+ 1)

--------------------------------------------------------------------------------

this :: Traversal (Maybe a) a
this _ Nothing = pure Nothing
this f (Just a) = Just <$> f a

elems :: Traversal [a] a
elems _ [] = pure []
elems f (a : as) = (:) <$> f a <*> elems f as

--------------------------------------------------------------------------------

listOf :: Monoid b => Traversal a b -> a -> b
listOf t a = unC $ t MkC a

points :: Traversal Point Double
points f (Point a b) = Point <$> f a <*> f b

both :: Traversal (a, a) a
both f (a, b) = (,) <$> f a <*> f b

--------------------------------------------------------------------------------

positiveOnly :: Double -> Maybe Double
positiveOnly a = if a < 0 then Nothing else Just a

--------------------------------------------------------------------------------
