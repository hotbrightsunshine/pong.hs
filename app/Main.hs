module Main where

import Graphics.Gloss

window :: Display
window = InWindow "Pong" (w, h) (offset, offset) 
    where h = 300
          w = 500
          offset = 1

background :: Color
background = white

drawing :: Picture
drawing = pictures [  circleSolid 20
                    , rectangleSolid 40 20 ]

main :: IO ()
main = display window background drawing
