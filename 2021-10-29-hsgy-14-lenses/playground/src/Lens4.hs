{-# LANGUAGE RankNTypes #-}

module Lens4 where

import Types

--------------------------------------------------------------------------------

-- Does not compile
-- askX :: Atom -> IO Atom
-- askX = over (point `comp` x) askUser
--   where
--     askUser :: Double -> IO Double
--     askUser a = do
--       putStrLn $ "Current position is " ++ show a ++ ". New Position?"
--       read <$> getLine

--------------------------------------------------------------------------------

-- data Lens a b = Lens
--   { view :: a -> b,
--     over :: (b -> b) -> (a -> a),
--     overIO :: (b -> IO b) -> (a -> IO a)
--   }

-- set :: Lens a b -> b -> a -> a
-- set l b = over l (const b)

-- mkLens :: (a -> b) -> (b -> a -> a) -> Lens a b
-- mkLens v s = Lens v o oIO
--   where
--     o f a = s (f (v a)) a
--     oIO f a = do
--       b' <- f (v a)
--       return $ s b' a

-- comp :: Lens a b -> Lens b c -> Lens a c
-- comp l1 l2 =
--   Lens
--     (view l2 . view l1)
--     (over l1 . over l2)
--     (overIO l1 . overIO l2)

--------------------------------------------------------------------------------

data Lens a b = Lens
  { view :: a -> b,
    over :: (b -> b) -> (a -> a),
    overF :: forall t. Functor t => (b -> t b) -> (a -> t a)
  }

set :: Lens a b -> b -> a -> a
set l b = over l (const b)

setF :: Functor f => Lens a b -> f b -> a -> f a
setF l b = overF l (const b)

mkLens :: (a -> b) -> (b -> a -> a) -> Lens a b
mkLens v s = Lens v o oF
  where
    o f a = s (f (v a)) a
    oF f a = (`s` a) <$> f (v a)

comp :: Lens a b -> Lens b c -> Lens a c
comp l1 l2 =
  Lens
    (view l2 . view l1)
    (over l1 . over l2)
    (overF l1 . overF l2)

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

askX :: Atom -> IO Atom
askX = overF (point `comp` x) askUser
  where
    askUser :: Double -> IO Double
    askUser a = do
      putStrLn $ "Current position is " ++ show a ++ ". New Position?"
      read <$> getLine

--------------------------------------------------------------------------------
