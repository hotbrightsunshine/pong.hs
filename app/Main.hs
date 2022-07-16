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
drawing = pictures [  color circleColor $ circleSolid 10
                    , color rectColor $ rectangleSolid 20 80 ]
          where circleColor = light red
                rectColor = dark blue

main :: IO ()
main = display window background drawing
