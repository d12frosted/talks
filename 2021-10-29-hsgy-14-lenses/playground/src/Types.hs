module Types where

--------------------------------------------------------------------------------

data Atom = Atom {_element :: String, _point :: Point} deriving (Show)

setPoint :: Point -> Atom -> Atom
setPoint p a = a {_point = p}

setElement :: String -> Atom -> Atom
setElement e a = a {_element = e}

--------------------------------------------------------------------------------

data Point = Point {_x :: Double, _y :: Double} deriving (Show)

setX :: Double -> Point -> Point
setX x p = p {_x = x}

setY :: Double -> Point -> Point
setY y p = p {_y = y}

--------------------------------------------------------------------------------
