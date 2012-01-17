module Graphics.X11.Turtle (
	initTurtle,

	forward,
	backward,
	left,
	right,
	circle,

	penup,
	pendown,
	isdown,

	shapesize,
	goto,
	home,
	clear,

	windowWidth,
	windowHeight,
	position,
	distance,

	undo,
	getHistory
) where

import qualified Graphics.X11.TurtleBase as Base
import Data.IORef
import Data.List
import System.IO.Unsafe
import Control.Monad
import Control.Concurrent
import Data.Maybe

turtle :: IORef Base.Turtle
turtle = unsafePerformIO $ newIORef undefined

initTurtle :: IO ()
initTurtle = Base.openWorld >>= Base.initTurtle >>= writeIORef turtle

shapesize :: Double -> IO ()
shapesize s = readIORef turtle >>= flip Base.shapesize s

windowWidth :: IO Double
windowWidth = readIORef turtle >>= Base.windowWidth

windowHeight :: IO Double
windowHeight = readIORef turtle >>= Base.windowHeight

position :: IO (Double, Double)
position = readIORef turtle >>= Base.position

distance :: Double -> Double -> IO Double
distance x0 y0 = do
	(x, y) <- position
	return $ ((x - x0) ** 2 + (y - y0) ** 2) ** (1 / 2)

penup :: IO ()
penup = readIORef turtle >>= Base.penup >> pushTurtleEvent Penup

pendown :: IO ()
pendown = readIORef turtle >>= Base.pendown >> pushTurtleEvent Pendown

isdown :: IO Bool
isdown = readIORef turtle >>= Base.isdown

goto, goto' :: Double -> Double -> IO ()
rawGoto :: Base.Turtle -> Double -> Double -> IO ()
goto x y = goto' x y >> setUndoPoint >> pushTurtleEvent (Goto x y)
goto' x y = do
	width <- windowWidth
	height <- windowHeight
	t <- readIORef turtle
	rawGoto t (x + width / 2) (- y + height / 2)

rawGoto t xTo yTo = do
	(act, past) <- rawGotoGen t xTo yTo
	act
	dir <- Base.getDirection t
	forM_ past $ \(pos, act') -> putToPastDrawLines pos dir act'

rawGotoGen :: Base.Turtle -> Double -> Double ->
	IO (IO (), [((Double, Double), Base.Buf -> IO ())])
rawGotoGen t xTo yTo = do
	(act, past) <- gotoGen t xTo yTo
	return (act, map (uncurry $ mkAction t) $ zip past $ tail past)

getSteps :: Double -> Double -> Double -> Double -> [(Double, Double)]
getSteps x0 y0 x y = let
	dist = ((x - x0) ** 2 + (y - y0) ** 2) ** (1 / 2)
	dx = step * (x - x0) / dist
	dy = step * (y - y0) / dist
	xs = takeWhile (aida x0 x) (map ((x0 +) . (* dx)) [0 ..]) ++ [x]
	ys = takeWhile (aida y0 y) (map ((y0 +) . (* dy)) [0 ..]) ++ [y] in
	zip xs ys

aida :: Ord a => a -> a -> a -> Bool
aida xs xe x = xs <= x && x <= xe || xs >= x && x >= xe

step :: Double
step = 10

gotoGen :: Base.Turtle -> Double -> Double -> IO (IO (), [(Double, Double)])
gotoGen t x y = do
	(x0, y0) <- Base.getPosition t
	let	poss = getSteps x0 y0 x y
		actss = mapM_ (uncurry $ Base.moveTurtle t) $ tail poss
	return (actss, poss)

mkAction :: Base.Turtle -> (Double, Double) -> (Double, Double) ->
	((Double, Double), Base.Buf -> IO ())
mkAction t (x0, y0) (x1, y1) = ((x1, y1), Base.drawLine t x0 y0 x1 y1)

forward, rawForward :: Double -> IO ()
forward len = rawForward len >> setUndoPoint >> pushTurtleEvent (Forward len)
rawForward len = do
	t <- readIORef turtle
	(x0, y0) <- Base.getPosition t
	d <- Base.getDirection t
	let	rad = d * pi / 180
		nx' = x0 + len * cos rad
		ny' = y0 + len * sin rad
	rawGoto t nx' ny'

backward :: Double -> IO ()
backward = forward . negate

rotateBy :: Double -> IO ()
rotateBy dd = do
	t <- readIORef turtle
	nd <- Base.rotateBy t dd
	pos <- Base.getPosition t
	putToPastDrawLines pos nd $ const (return ())

rotateTo :: Double -> IO ()
rotateTo d = do
	t <- readIORef turtle
	d0 <- Base.getDirection t
	let	st = 5
		dd = d - d0
	replicateM_ (abs dd `gDiv` st) $
		rotateBy (signum dd * st) >> threadDelay 10000
	Base.setDirection t d
	Base.flushW t

rotate, rawRotate :: Double -> IO ()
rotate d = rawRotate d >> setUndoPoint >> pushTurtleEvent (Rotate d)
rawRotate d = do
	d0 <- readIORef turtle >>= Base.getDirection
	rotateTo $ d0 + d

gDiv :: (Num a, Ord a, Integral b) => a -> a -> b
x `gDiv` y
	| x >= y = 1 + (x - y) `gDiv` y
	| otherwise = 0

right :: Double -> IO ()
right = rotate

left :: Double -> IO ()
left = rotate . negate

circle :: Double -> IO ()
circle r = replicateM_ 36 $ do
	rawForward (2 * r * pi / 36 :: Double)
	rawRotate (- 10)

home :: IO ()
home = goto' 0 0 >> rotateTo 0 >> pushTurtleEvent Home

clear :: IO ()
clear = do
	t <- readIORef turtle
	(retAct, (pos, dir, pastAct)) <- Base.clear t
	retAct
	putToPastDrawLines pos dir pastAct
	pushTurtleEvent Clear

--------------------------------------------------------------------------------

undo :: IO ()
undo = do
	t <- readIORef turtle
	dls <- fmap init $ readIORef pastDrawLines
	let	draw = map catMaybes
			$ takeWhile (isJust . last) $ reverse $ inits dls
		draw1 = map fromJust $ filter isJust $ head
			$ dropWhile (isJust . last) $ reverse $ inits dls
		draw' = draw ++ [draw1]
	forM_ (zip (reverse $ map (fst . fromJust) $ filter isJust dls )
		$ map (mapM_ (($ Base.BG) . snd)) draw') $ \((pos, dir), dl) -> do
		Base.initUndo t
		dl
		uncurry (Base.setPosition t) pos
		Base.setDirection t dir
		Base.flushW t
		threadDelay 20000
	modifyIORef pastDrawLines $ reverse . dropWhile isJust . tail . reverse
	pushTurtleEvent Undo

data TurtleEvent = Forward Double | Rotate Double | Undo | Home | Clear
	| Penup | Pendown | Goto Double Double
	deriving Show

getHistory :: IO [TurtleEvent]
getHistory = liftM (`take` turtleEvents) $ readIORef eventPoint 

eventChan :: Chan TurtleEvent
eventChan = unsafePerformIO newChan

turtleEvents :: [TurtleEvent]
turtleEvents = unsafePerformIO getTurtleEvents

getTurtleEvents :: IO [TurtleEvent]
getTurtleEvents = unsafeInterleaveIO $ do
	ev <- readChan eventChan
	evs <- getTurtleEvents
	return $ ev : evs

eventPoint :: IORef Int
eventPoint = unsafePerformIO $ newIORef 0

pushTurtleEvent :: TurtleEvent -> IO ()
pushTurtleEvent te = do
	writeChan eventChan te
	modifyIORef eventPoint (+ 1)

pastDrawLines :: IORef [Maybe (((Double, Double), Double), Base.Buf -> IO ())]
pastDrawLines = unsafePerformIO $ newIORef []

putToPastDrawLines :: (Double, Double) -> Double -> (Base.Buf -> IO ()) -> IO ()
putToPastDrawLines tpos tdir dl = do
	pdls <- readIORef pastDrawLines
	if length pdls < 300
		then writeIORef pastDrawLines $ pdls ++ [Just ((tpos, tdir), dl)]
		else do
			maybe (return ()) (($ Base.UndoBuf) . snd) $ head pdls
			writeIORef pastDrawLines $ tail pdls ++ [Just ((tpos, tdir), dl)]

setUndoPoint :: IO ()
setUndoPoint = modifyIORef pastDrawLines (++ [Nothing])
