module Main where

import Graphics.UI.GLUT.Turtle
import Graphics.UI.GLUT.Turtle.Field
import Graphics.UI.GLUT(initialize, addTimerCallback, mainLoop)
import System.Random
import System.Environment
import Control.Monad
import Control.Concurrent
import Data.Word

main :: IO ()
main = do
	prgName <- getProgName
	rawArgs <- getArgs
	args <- initialize prgName rawArgs
	f <- openField
	t <- newTurtle f
--	pencolor t "white"
	pensize t 3
	pencolor t (255, 255, 255)
	preprocess t
	(x0, y0) <- position t
	addTimerCallback 100 $ timerProc $ draw t x0 y0
	mainLoop

timerProc act = do
	act
	addTimerCallback 20 $ timerProc act

randomWord8 :: IO Word8
randomWord8 = fmap fromIntegral $ (randomRIO (0, 255) :: IO Int)

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

draw t x0 y0 = do
	d <- randomRIO (- 180, 180)
	r <- randomWord8
	g <- randomWord8
	b <- randomWord8
	pencolor t (r, g, b)
	left t d
	forward t 15
	d <- distance t x0 y0
	when (d > 100) $ undo t
