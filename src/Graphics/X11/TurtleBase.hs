module Graphics.X11.TurtleBase (
	module Graphics.X11.World,
	displayTurtle,
	Turtle(..),
	initTurtle,
	shapesize,
	windowWidth,
	windowHeight,
	position,

	penup,
	pendown,
	isdown,
	Buf(..),
	drawLine,
	rawGotoGen
) where

import Graphics.X11.World
import Control.Arrow
import System.IO.Unsafe
import Data.IORef
import Control.Monad
import Control.Monad.Tools
import Control.Concurrent

data Turtle = Turtle{tWorld :: World}

initTurtle :: IO Turtle
initTurtle = do
	w <- openWorld
	setCursorPos w 100 200
	setCursorDir w 0
	setCursorSize w 2
	setCursorShape w displayTurtle
	drawWorld w
	flushWorld $ wWin w
	(width, height) <- winSize $ wWin w
	setCursorPos w (width / 2) (height / 2)
	drawWorld w
	flushWorld $ wWin w
	return $ Turtle w

shapesize :: Turtle -> Double -> IO ()
shapesize t s = do
	let w = tWorld t
	setCursorSize w s
	drawWorld w
	flushWorld $ wWin w

position :: Turtle -> IO (Double, Double)
position t = do
	let w = tWorld t
	(x, y) <- getCursorPos w
	width <- windowWidth t
	height <- windowHeight t
	return (x - width / 2, height / 2 - y)

windowWidth :: Turtle -> IO Double
windowWidth = fmap fst . winSize . wWin . tWorld

windowHeight :: Turtle -> IO Double
windowHeight = fmap snd . winSize . wWin . tWorld

data PenState = PenUp | PenDown

penState :: IORef PenState
penState = unsafePerformIO $ newIORef PenDown

penup :: Turtle -> IO ()
penup _ = writeIORef penState PenUp

pendown :: Turtle -> IO ()
pendown _ = writeIORef penState PenDown

isdown :: Turtle -> IO Bool
isdown _ = do
	ps <- readIORef penState
	return $ case ps of
		PenUp -> False
		PenDown -> True

data Buf = BG | UndoBuf

rawGotoGen :: Turtle -> Double -> Double ->
	IO (IO (), [((Double, Double), Buf -> IO ())])
rawGotoGen t xTo yTo = do
	let w = tWorld t
	(x0, y0) <- getCursorPos w
	let	step = 10
		distX = xTo - x0
		distY = yTo - y0
		dist = (distX ** 2 + distY ** 2) ** (1 / 2)
		dx = step * distX / dist
		dy = step * distY / dist
	acts <- newIORef $ return ()
	pasts <- newIORef []
	(x', y') <- if dist <= step then return (x0, y0) else
		doWhile (x0, y0) $ \(x, y) -> do
			let	nx = x + dx
				ny = y + dy
			modifyIORef acts (>> moveTurtle t x y nx ny)
			mkPastDrawLineWithDir nx ny (drawLine t x y nx ny) >>=
				modifyIORef pasts . (:)
			return ((nx, ny),
				(nx + dx - x0) ** 2 + (ny + dy - y0) ** 2 < dist ** 2)
	lastPastAct <- mkPastDrawLineWithDir xTo yTo (drawLine t x' y' xTo yTo)
	pastActs <- fmap ((++[lastPastAct]) . reverse) $ readIORef pasts
	return (join (readIORef acts) >> moveTurtle t x' y' xTo yTo, pastActs)

mkPastDrawLineWithDir :: Double -> Double -> (Buf -> IO ()) ->
	IO ((Double, Double), Buf -> IO ())
mkPastDrawLineWithDir x2 y2 act = do
	return ((x2, y2), act)

moveTurtle :: Turtle -> Double -> Double -> Double -> Double -> IO ()
moveTurtle t x1 y1 x2 y2 = do
	let w = tWorld t
	setCursorPos w x2 y2
	drawLine t x1 y1 x2 y2 BG
	drawWorld w
	flushWorld $ wWin w
	threadDelay 20000

drawLine :: Turtle -> Double -> Double -> Double -> Double -> Buf -> IO ()
drawLine t x1 y1 x2 y2 buf = do
		let w = tWorld t
		pd <- isdown t
		when pd $
			case buf of
				BG -> lineToBG (wWin w) x1 y1 x2 y2
				UndoBuf -> lineToUndoBuf (wWin w) x1 y1 x2 y2

displayTurtle :: Win -> Double -> Double -> Double -> Double -> IO ()
displayTurtle w s d x y =
	makeFilledPolygonCursor w $ map (uncurry $ addDoubles (x, y))
		$ map (rotatePointD d)
		$ map (mulPoint s) turtle

turtle :: [(Double, Double)]
turtle = ttl ++ reverse (map (second negate) ttl)
	where
	ttl = [
		(- 10, 0),
		(- 8, - 3),
		(- 10, - 5),
		(- 7, - 9),
		(- 5, - 6),
		(0, - 8),
		(4, - 7),
		(6, - 10),
		(8, - 7),
		(7, - 5),
		(10, - 2),
		(13, - 3),
		(16, 0)
	 ]

addDoubles :: (Double, Double) -> Double -> Double -> (Double, Double)
addDoubles (x, y) dx dy = (x + dx, y + dy)

rotatePointD :: Double -> (Double, Double) -> (Double, Double)
rotatePointD = rotatePointR . (* pi) . (/ 180)

rotatePointR :: Double -> (Double, Double) -> (Double, Double)
rotatePointR rad (x, y) =
	(x * cos rad - y * sin rad, x * sin rad + y * cos rad)

mulPoint :: Double -> (Double, Double) -> (Double, Double)
mulPoint s (x, y) = (x * s, y * s)