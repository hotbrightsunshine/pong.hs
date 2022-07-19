module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact


isIn :: Ord a => a -> (a, a) -> Bool
isIn z (x, y) = z >= x && z <= y


window :: Display
window = InWindow "Pong" (w, h) (offset, offset) 
    where h = 300
          w = 500
          offset = 1


background :: Color
background =  black


data Pong = Game {
    ballPos ::  (Float, Float)
,   ballVel ::  (Float, Float)
,   player1 ::  Float
,   player2 ::  Float
,   pause   ::  Bool
} deriving Show


initial :: Pong
initial = Game {
    ballPos =   (180, 40)
,   ballVel =   (50, -150)
,   player1 =   60
,   player2 =   0
,   pause = False
}


outOfBounds :: (Float, Float) -> Pong -> Pong
outOfBounds dims game = if (fst (ballPos game)) `isIn` (-(fst dims) / 2, fst dims / 2) &&
                           (snd (ballPos game)) `isIn` (-(snd dims) / 2, snd dims / 2)
                        then game
                        else error "Out of bounds!"


render :: Pong -> Picture
render  game =  pictures    [ ball, walls,
                            paddle1, paddle2 ]
                where 
                    -- Ball
                    ball =  uncurry     translate (ballPos game)
                                        $ color red 
                                        $ circleSolid 10

                    -- Walls
                    wall :: Float -> Picture 
                    wall offset =       color white 
                                        $ translate 0 offset
                                        $ rectangleSolid 400 10
                    
                    walls = pictures [ wall (-150), wall 150 ]

                    -- Paddles
                    makePaddle :: Color -> Float -> Float -> Picture 
                    makePaddle col x offset = translate x offset 
                                              $ color col $ rectangleSolid 20 80

                    paddle1 = makePaddle white (-225) (player1 game)
                    paddle2 = makePaddle white 225 (player2 game)

moveBall :: Float -> Pong -> Pong 
moveBall time game = if not (pause game)
                        then
                            game { ballPos = (x', y') }
                        else
                            game
                        where
                            (x, y) = ballPos game
                            (vx, vy) = ballVel game

                            x' = x + vx * time
                            y' = y + vy * time


wallCollision :: Float -> (Float, Float) -> Bool
wallCollision rad pos = upper || lower
    where
        upper = snd pos + rad >= 150 - (10 / 2)
        lower = snd pos - rad <= (-150) + (10 / 2)


wallCollided :: Pong -> Pong
wallCollided game = game { ballVel = (vx, vy') }
    where
        (vx, vy) = ballVel game
        vy' = if wallCollision 10 (ballPos game)
                 then -vy
                 else vy


paddleCollision :: Float -> (Float, Float) -> Pong -> Bool
paddleCollision rad pos game = left || right
    where
        right  = (fst pos + rad) `isIn` (225-(20/2), 225+(20/2)) &&
            snd pos `isIn` (player2 game - 40 , player2 game + 40)

        left = (fst pos - rad) `isIn` ((-225)-(20/2), (-225)+(20/2))   &&
            snd pos `isIn` (player1 game - 40 , player1 game + 40)


paddleCollided :: Pong -> Pong
paddleCollided game = game { ballVel = (vx', vy) }
    where
        (vx, vy) = ballVel game
        vx' = if paddleCollision 10 (ballPos game) game
                then -vx
                else vx

movePaddle1 :: Pong -> Float -> Float -> Float -> Pong
movePaddle1 game incr bound pheight = game { player1 = pos' }
    where
        pos' = if (player1 game + incr + (pheight / 2) >= bound / 2)
                  || (player1 game + incr - (pheight / 2) <= -(bound / 2))
                then player1 game
                else player1 game + incr

movePaddle2 :: Pong -> Float -> Float -> Float -> Pong
movePaddle2 game incr bound pheight = game { player2 = pos' }
    where
        pos' = if (player2 game + incr + (pheight / 2) >= bound / 2)
                  || (player2 game + incr - (pheight / 2) <= -(bound / 2))
                then player2 game
                else player2 game + incr


event :: Event -> Pong -> Pong
event (EventKey (Char 'p') Down _ _) game = game { pause = not (pause game) }
event (EventKey (Char 's') Down _ _) game = if pause game
                                                then
                                                    game
                                                else
                                                    movePaddle1 game (-10) 300 80
event (EventKey (Char 'w') Down _ _) game = if pause game
                                                then
                                                    game
                                                else
                                                    movePaddle1 game (10) 300 80

event (EventKey (SpecialKey KeyDown) Down _ _) game = if pause game
                                                then
                                                    game
                                                else
                                                    movePaddle2 game (-10) 300 80
event (EventKey (SpecialKey KeyUp) Down _ _) game = if pause game
                                                then
                                                    game
                                                else
                                                    movePaddle2 game (10) 300 80
event _ game = game

main :: IO ()
main = play window background 75 initial render event update
    where 
        update :: Float -> Pong -> Pong
        update seconds = outOfBounds (300, 500) . wallCollided . paddleCollided . moveBall seconds
