{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

--------------------------------------------------------------------------------

module Main where

--------------------------------------------------------------------------------

import Data.Tree
import Diagrams.Backend.Rasterific
import Diagrams.Backend.Rasterific.CmdLine
import Diagrams.Prelude
import Diagrams.TwoD.Layout.Tree
import Diagrams.TwoD.Size

--------------------------------------------------------------------------------

tFoldl :: Tree String
tFoldl = Node "+"
     [ Node "+"
       [ Node "+"
         [ Node "+"
           [ Node "z" []
           , Node "x1" []
           ]
         , Node "x2" []
         ]
       , Node "x3" []
       ]
     , Node "x4" []
     ]

tFoldr :: Tree String
tFoldr = Node "+"
     [ Node "x1" []
     , Node "+"
       [ Node "x2" []
       , Node "+"
         [ Node "x3" []
         , Node "+"
           [ Node "x4" []
           , Node "z" []
           ]
         ]
       ]
     ]

--------------------------------------------------------------------------------

symmTree :: Tree String -> Diagram B
symmTree t = vcat
  [ renderTree ((<> circle 1.2 # fc white) . text)
    (~~)
    (symmLayout' (with & slHSep .~ 4 & slVSep .~ 4) t)
    # centerXY # pad 1.2
  ]

--------------------------------------------------------------------------------

main :: IO ()
main = do
  renderRasterific "out/foldl-tree.png" (mkWidth 500) $ symmTree tFoldl
  renderRasterific "out/foldr-tree.png" (mkWidth 500) $ symmTree tFoldr

--------------------------------------------------------------------------------
