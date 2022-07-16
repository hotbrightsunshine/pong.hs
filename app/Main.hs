module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

window :: Display
window = InWindow "Pong" (w, h) (offset, offset) 
    where h = 300
          w = 500
          offset = 1

background :: Color
background =    black


data Pong = Game {
    ballPos ::  (Float, Float)
,   ballVel ::  (Float, Float)
,   player1 ::  Float
,   player2 ::  Float
} deriving Show


initial :: Pong
initial = Game {
    ballPos =   (0, 0)
,   ballVel =   (1, -3)
,   player1 =   0
,   player2 =   0
}


render :: Pong -> Picture
render  game =  pictures    [ ball, walls,
                            paddle1, paddle2 ]
                where 
                    -- Ball
                    ball = translate    (fst (ballPos game))
                                        (snd (ballPos game))
                                        $ color red 
                                        $ circleSolid 10

                    -- Walls
                    wall :: Float -> Picture 
                    wall offset =       color white 
                                        $ translate 0 (offset) 
                                        $ rectangleSolid 400 10
                    
                    walls = pictures [ wall (-150), wall 150 ]

                    -- Paddles
                    makePaddle :: Color -> Float -> Float -> Picture 
                    makePaddle col x offset = translate x offset 
                                              $ color col $ rectangleSolid 20 80

                    paddle1 = makePaddle blue (-225) (player1 game) 
                    paddle2 = makePaddle blue 225 (player2 game) 

moveBall :: Float -> Pong -> Pong 
moveBall time game = game { ballPos = (x', y') }
                     where 
                        (x, y) = ballPos game
                        (vx, vy) = ballVel game

                        x' = x + vx * time
                        y' = y + vy * time

main :: IO ()
main = simulate background 60 initial render update
    where 
        update :: ViewPort -> Float -> Pong -> Pong
        update _ = moveBall
