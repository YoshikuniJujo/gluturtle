module NewTurtle (
	openField,
	newTurtle,
	Turtle,
	shape,
	shapesize,
	forward,
	left,
	undo,
	position,
	distance,
	home,
	circle
) where

import TurtleDraw
import TurtleInput
import Control.Concurrent
import Control.Monad
import Control.Monad.Tools
import Prelude hiding (Left)
import Data.IORef

data Turtle = Turtle {
	inputChan :: Chan TurtleInput,
	field :: Field,
	layer :: Layer,
	character :: Character,
	states :: [TurtleState],
	stateNow :: IORef Int
 }

newTurtle :: Field -> IO Turtle
newTurtle f = do
	ch <- addCharacter f
	l <- addLayer f
	(c, ret) <- makeInput
	sn <- newIORef 1
	let	sts = drop 4 $ inputToTurtle [] initialTurtleState ret
		t = Turtle {
			inputChan = c,
			field = f,
			layer = l,
			character = ch,
			states = sts,
			stateNow = sn
		 }
	writeChan c $ Shape classic
	writeChan c $ ShapeSize 1
	writeChan c $ PenDown
	writeChan c $ Goto 0 0
	writeChan c $ RotateTo 0
	writeChan c $ Goto 0 0
	forkIO $ do
--		initialThread
		for2M_ sts $ turtleDraw f ch l
	return t

shape :: Turtle -> String -> IO ()
shape Turtle{inputChan = c} "turtle" = writeChan c $ Shape turtle
shape Turtle{inputChan = c} "classic" = writeChan c $ Shape classic

shapesize :: Turtle -> Double -> IO ()
shapesize Turtle{inputChan = c} = writeChan c . ShapeSize

forward, backward :: Turtle -> Double -> IO ()
forward Turtle{inputChan = c, stateNow = sn} len = do
	modifyIORef sn (+1)
	writeChan c $ Forward len
backward t = forward t . negate

left, right :: Turtle -> Double -> IO ()
left Turtle{inputChan = c, stateNow = sn} dd = do
	modifyIORef sn (+ 1)
	writeChan c $ Left dd
right t = left t . negate

circle :: Turtle -> Double -> IO ()
circle t r = do
	forward t (r * pi / 36)
	left t 10
	replicateM_ 35 $ forward t (2 * r * pi / 36) >> left t 10
	forward t (r * pi / 36)

home :: Turtle -> IO ()
home t = modifyIORef (stateNow t) (+ 1) >> goto t 0 0 >> rotateTo t 0

position :: Turtle -> IO (Double, Double)
position t@Turtle{stateNow = sn, states = s} =
	fmap (turtlePos . (s !!)) $ readIORef sn

distance :: Turtle -> Double -> Double -> IO Double
distance t x0 y0 = do
	(x, y) <- position t
	return $ ((x - x0) ** 2 + (y - y0) ** 2) ** (1 / 2)

goto :: Turtle -> Double -> Double -> IO ()
goto Turtle{inputChan = c} x y = writeChan c $ Goto x y

rotateTo :: Turtle -> Double -> IO ()
rotateTo Turtle{inputChan = c} d = writeChan c $ RotateTo d

undo :: Turtle -> IO ()
undo Turtle{inputChan = c, stateNow = sn} = do
	modifyIORef sn (+1)
	writeChan c Undo

for2M_ :: [a] -> (a -> a -> IO b) -> IO ()
for2M_ xs f = zipWithM_ f xs $ tail xs

main :: IO ()
main = do
	putStrLn "module NewTurtle"

	f <- openField
	ch <- addCharacter f
	l <- addLayer f

	(c, ret) <- makeInput
	writeChan c $ Shape classic
	writeChan c $ ShapeSize 1
	writeChan c $ PenDown
	writeChan c $ Goto 0 0
	writeChan c $ RotateTo 0
	writeChan c $ RotateTo 180
	writeChan c $ Goto 50 100
	writeChan c $ Goto 100 50
	writeChan c $ Goto 100 100
	writeChan c $ RotateTo (- 180)
	writeChan c Undo
	writeChan c Undo
	writeChan c Undo
	print $ take 12 ret
	let turtles = inputToTurtle [] initialTurtleState ret
	print $ drop 4 $ take 12 turtles
	turtleDraw f ch l (turtles !! 4) (turtles !! 5)
	turtleDraw f ch l (turtles !! 5) (turtles !! 6)
	turtleDraw f ch l (turtles !! 6) (turtles !! 7)
	turtleDraw f ch l (turtles !! 7) (turtles !! 8)
	turtleDraw f ch l (turtles !! 8) (turtles !! 9)
	turtleDraw f ch l (turtles !! 9) (turtles !! 10)
	turtleDraw f ch l (turtles !! 10) (turtles !! 11)
	turtleDraw f ch l (turtles !! 11) (turtles !! 12)

testUndo :: IO ()
testUndo = do
	f <- openField
	t <- newTurtle f
	goto t 100 100
	goto t 100 200
	rotateTo t 180
	undo t
	undo t
	mapM_ print $ take 6 $ states t