--------------------------------------------------------------------------------

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Data.GOL where

--------------------------------------------------------------------------------

import           Data.Display.Extra
import           Data.Universe
import           Data.Universe2

--------------------------------------------------------------------------------

import           Control.Comonad
import           RIO
import           RIO.List           (findIndex, intersperse, iterate)
import           RIO.List.Partial   (tail)

--------------------------------------------------------------------------------

data Cell
  = Dead
  | Alive
  deriving (Eq)

instance Display Cell where
  display Dead  = "□"
  display Alive = "■"

--------------------------------------------------------------------------------

rule :: U2 Cell -> Cell
rule u | ns == 3   = Alive
       | ns == 2   = extract u
       | otherwise = Dead
  where ns = length . filter (== Alive) . neighbours Dead $ u

--------------------------------------------------------------------------------

neighbours :: Cell -> U2 Cell -> [Cell]
neighbours c u
  = fmap (\f -> extract $ f u)
    [ shift North
    , shift North . shift East
    , shift East
    , shift East . shift South
    , shift South
    , shift South . shift West
    , shift West
    , shift West . shift North
    ]

{-
all shifts can be calculated directly:

allShifts = [ shift North
            , shift North . shift East
            , shift East
            , shift East . shift South
            , shift South
            , shift South . shift West
            , shift West
            , shift West . shift North
            ]

or using applicative for list

allShifts = fmap (appm shift) . tail $ (<>) <$> [[], [North], [South]] <*> [[], [East], [West]]
  where appm f args = foldl' (\g a -> g . f a) id args

I chose more obvious way :D
-}

--------------------------------------------------------------------------------

gol1 :: U2 Cell
gol1
  = fromList2 Dead
  [ [ Dead, Dead, Dead, Dead, Dead, Dead, Dead ]
  , [ Dead, Alive, Alive, Alive, Alive, Alive, Dead ]
  , [ Dead, Alive, Alive, Dead, Alive, Alive, Dead ]
  , [ Dead, Alive, Alive, Dead, Alive, Alive, Dead ]
  , [ Dead, Alive, Alive, Dead, Alive, Alive, Dead ]
  , [ Dead, Alive, Alive, Alive, Alive, Alive, Dead ]
  , [ Dead, Dead, Dead, Dead, Dead, Dead, Dead ]
  ]

gol2 :: U2 Cell
gol2
  = fromList2 Dead
  [ [ Dead, Alive, Dead ]
  , [ Alive, Dead, Dead ]
  , [ Alive, Alive, Alive ]
  ]

gol3 :: U2 Cell
gol3
  = fromList2 Dead
  [ [ Dead, Dead, Dead, Dead, Dead ]
  , [ Dead, Dead, Alive, Dead, Dead ]
  , [ Dead, Dead, Alive, Dead, Dead ]
  , [ Dead, Dead, Alive, Dead, Dead ]
  , [ Dead, Dead, Dead, Dead, Dead ]
  ]

gol4 :: U2 Cell
gol4
  = fromList2 Dead
  [ [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ]
  , [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ]
  , [ Dead, Dead, Dead, Dead, Alive, Alive, Alive, Dead, Dead, Dead, Alive, Alive, Alive, Dead, Dead, Dead, Dead ]
  , [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ]
  , [ Dead, Dead, Alive, Dead, Dead, Dead, Dead, Alive, Dead, Alive, Dead, Dead, Dead, Dead, Alive, Dead, Dead ]
  , [ Dead, Dead, Alive, Dead, Dead, Dead, Dead, Alive, Dead, Alive, Dead, Dead, Dead, Dead, Alive, Dead, Dead ]
  , [ Dead, Dead, Alive, Dead, Dead, Dead, Dead, Alive, Dead, Alive, Dead, Dead, Dead, Dead, Alive, Dead, Dead ]
  , [ Dead, Dead, Dead, Dead, Alive, Alive, Alive, Dead, Dead, Dead, Alive, Alive, Alive, Dead, Dead, Dead, Dead ]
  , [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ]
  , [ Dead, Dead, Dead, Dead, Alive, Alive, Alive, Dead, Dead, Dead, Alive, Alive, Alive, Dead, Dead, Dead, Dead ]
  , [ Dead, Dead, Alive, Dead, Dead, Dead, Dead, Alive, Dead, Alive, Dead, Dead, Dead, Dead, Alive, Dead, Dead ]
  , [ Dead, Dead, Alive, Dead, Dead, Dead, Dead, Alive, Dead, Alive, Dead, Dead, Dead, Dead, Alive, Dead, Dead ]
  , [ Dead, Dead, Alive, Dead, Dead, Dead, Dead, Alive, Dead, Alive, Dead, Dead, Dead, Dead, Alive, Dead, Dead ]
  , [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ]
  , [ Dead, Dead, Dead, Dead, Alive, Alive, Alive, Dead, Dead, Dead, Alive, Alive, Alive, Dead, Dead, Dead, Dead ]
  , [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ]
  , [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ]
  ]

{-

someGOL :: U2 Cell
someGOL
  = fromList2 Dead
  [ [ Dead, Dead, Dead, Dead, Dead ]
  , [ Dead, Dead, Alive, Dead, Dead ]
  , [ Dead, Dead, Alive, Dead, Dead ]
  , [ Dead, Dead, Alive, Dead, Dead ]
  , [ Dead, Dead, Dead, Dead, Dead ]
  ]

λ mapM_ (print . narrow2' 0 4 0 4) . take 10 . iterate (extend rule) $ someGOL
-}

--------------------------------------------------------------------------------
-- Broken GOL

rule' :: U (U Cell) -> Cell
rule' u | ns == 3   = Alive
        | ns == 2   = extract $ extract u
        | otherwise = Dead
  where ns = length . filter (== Alive) . neighbours' Dead $ u

duplicate2 :: U (U a) -> U (U (U (U a)))
duplicate2 = fmap duplicate . duplicate

extend2 :: (U (U a) -> b) -> U (U a) -> U (U b)
extend2 f = fmap (fmap f) . duplicate2

neighbours' :: Cell -> U (U Cell) -> [Cell]
neighbours' c u
  = fmap (\f -> extract . extract $ f u)
    [ shift' North
    , shift' North . shift' East
    , shift' East
    , shift' East . shift' South
    , shift' South
    , shift' South . shift' West
    , shift' West
    , shift' West . shift' North
    ]

shift' North u = left      u
shift' South u = right     u
shift' East  u = right <$> u
shift' West  u = left  <$> u

--------------------------------------------------------------------------------
