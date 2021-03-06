module Main where

import Graphics.UI.GLUT.Turtle
import Graphics.UI.GLUT(addTimerCallback, mainLoop)
import System.Random
import Control.Monad
import Data.Word

main :: IO ()
main = do
	_args <- initialize
	f <- openField "random" 480 480
	t <- newTurtle f
--	pencolor t "white"
	pensize t 3
	pencolor t ((255, 255, 255) :: (Int, Int, Int))
	preprocess t
	(x0, y0) <- position t
	addTimerCallback 100 $ timerProc $ draw t x0 y0
	mainLoop

timerProc :: IO a -> IO ()
timerProc act = do
	_ <- act
	addTimerCallback 20 $ timerProc act

randomWord8 :: IO Word8
randomWord8 = fmap fromIntegral (randomRIO (0, 255) :: IO Int)

randomTurtle :: Turtle -> IO ()
randomTurtle t = do
	preprocess t
	(x0, y0) <- position t
	sequence_ $ repeat $ draw t x0 y0

preprocess :: Turtle -> IO ()
preprocess t = do
	penup t
	shape t "turtle"
	shapesize t 2 2
	(x0, y0) <- position t
	forward t 100
	pendown t
	left t 90
	circle t 100
	penup t
	goto t x0 y0
	pendown t
	position t >>= print

draw :: Turtle -> Double -> Double -> IO ()
draw t x0 y0 = do
	d <- randomRIO (- 180, 180)
	r <- randomWord8
	g <- randomWord8
	b <- randomWord8
	pencolor t (r, g, b)
	left t d
	forward t 15
	dist <- distance t x0 y0
	when (dist > 100) $ undo t
