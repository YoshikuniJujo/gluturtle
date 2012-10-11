{-# LANGUAGE DoRec #-}

module Graphics.UI.GLUT.Turtle.Field(
	initialize,

	-- * types and classes
	Field,
	Layer,
	Character,
	Coordinates(..),

	-- * basic functions
	openField,
	closeField,
	waitField,
	topleft,
	center,
	coordinates,
	fieldSize,
	setFieldSize,

	-- * draw
	forkField,
	flushField,
	fieldColor,

	-- ** to Layer
	drawLine,
	fillRectangle,
	fillPolygon,
	writeString,
	drawImage,
	undoLayer,
	undoField,
	clearLayer,

	-- ** to Character
	drawCharacter,
	drawCharacterAndLine,
	clearCharacter,

	outputString,

	-- * event driven
	oninputtext,
	onclick,
	onrelease,
	ondrag,
	onmotion,
	onkeypress,
	ontimer,

	addLayer,
	addCharacter,

	prompt
) where

import Control.Monad

import Graphics.UI.GLUT.Turtle.Triangles

import Graphics.UI.GLUT(
	createWindow, Vertex2(..), renderPrimitive, vertex, PrimitiveMode(..),
	preservingMatrix, GLfloat, swapBuffers, ($=), displayCallback,
	initialDisplayMode, initialWindowSize, Size(..),
	DisplayMode(..), flush, currentWindow, Window
 )
import qualified Graphics.UI.GLUT as G

import Graphics.UI.GLUT.Turtle.Layers(
	Layers, Layer, Character, newLayers,
	makeLayer, undoLayer, clearLayer,
	makeCharacter, character)
import Text.XML.YJSVG(Position(..), Color(..))

-- import Control.Monad(when, unless, forever, replicateM, forM_, join)
import Control.Concurrent(ThreadId, forkIO)
import Data.IORef(IORef, newIORef, readIORef, writeIORef)
import Data.IORef.Tools(atomicModifyIORef_)
import Data.Maybe
import System.Environment

--------------------------------------------------------------------------------

initialize :: IO [String]
initialize = do
	prgName <- getProgName
	rawArgs <- getArgs
	G.initialize prgName rawArgs

prompt f p = do
	writeIORef (fPrompt f) p
	atomicModifyIORef_ (fString f) (\ls -> init ls ++ [p ++ last ls])

data Coordinates = CoordTopLeft | CoordCenter

data Field = Field{
	fCoordinates :: Coordinates,

	fBgcolor :: IORef [Color],
	fAction :: IORef (IO ()),
	fActions :: IORef [Maybe (IO ())],

	fString :: IORef [String],
	fString2 :: IORef [String],

	fInputtext :: IORef (String -> IO Bool),

	fWidth :: IORef Int,
	fHeight :: IORef Int,

	fFieldWindow :: Window,
	fConsoleWindow :: Window,

	fPrompt :: IORef String,

	fLayers :: IORef Layers
 }

addLayer :: Field -> IO Layer
addLayer = makeLayer . fLayers
addCharacter :: Field -> IO Character
addCharacter = makeCharacter . fLayers

--------------------------------------------------------------------------------

undoField :: Field -> IO ()
undoField f = do
	a : _ <- readIORef $ fActions f
	when (isNothing a) $ atomicModifyIORef_ (fBgcolor f) tail
	atomicModifyIORef_ (fActions f) myTail

myTail [] = error "myTail failed"
myTail (x : xs) = xs

openField :: String -> Int -> Int -> IO Field
openField name w h = do
	fw <- newIORef w
	fh <- newIORef h
	layers <- newLayers 0 (return ()) (return ()) (return ())
	bgc <- newIORef $ [RGB 255 255 255]
	action <- newIORef $ return ()
	actions <- newIORef [] -- [makeFieldColor $ RGB 255 255 255]
	str <- newIORef [""]
	str2 <- newIORef []
	inputtext <- newIORef $ const $ return True

	prmpt <- newIORef ""

	initialDisplayMode $= [RGBMode, DoubleBuffered]
	initialWindowSize $= Size (fromIntegral w) (fromIntegral h)
	wt <- createWindow name
	wc <- createWindow "console"
	currentWindow $= Just wt
	displayCallback $= (sequence_ . catMaybes =<< readIORef actions)
	currentWindow $= Just wc
	displayCallback $= (sequence_ . catMaybes =<< readIORef actions)
	G.addTimerCallback 10 (timerAction $ do
		currentWindow $= Just wt
		G.clearColor $= G.Color4 0 0 0 0
		G.clear [G.ColorBuffer]
		makeFieldColor . head =<< readIORef bgc
		sequence_ . reverse . catMaybes =<< readIORef actions
		join $ readIORef action
		swapBuffers
		currentWindow $= Just wc
		G.clearColor $= G.Color4 0 0 0 0
		G.clear [G.ColorBuffer]
		G.lineWidth $= 1.0
		ss1 <- readIORef str
		ss2 <- readIORef str2
		zipWithM_ (printString (-2.8)) [-1800, -1600 .. 1800] (reverse ss1 ++ ss2)
		swapBuffers)
	G.reshapeCallback $= Just (\size -> G.viewport $= (G.Position 0 0, size))
	let f = Field{
		fCoordinates = CoordCenter,
		fLayers = layers,
		fAction = action,
		fActions = actions,
		fString = str,
		fString2 = str2,
		fWidth = fw,
		fHeight = fh,
		fInputtext = inputtext,
		fFieldWindow = wt,
		fConsoleWindow = wc,
		fPrompt = prmpt,

		fBgcolor = bgc
	 }
	G.keyboardMouseCallback $= Just (keyboardProc f)
	return f

printString :: GLfloat -> GLfloat -> String -> IO ()
printString x y str =
	preservingMatrix $ do
		G.scale (0.0005 :: GLfloat)  0.0005 0.0005
		G.clearColor $= G.Color4 0 0 0 0
		G.color (G.Color4 0 1 0 0 :: G.Color4 GLfloat)
		w <- G.stringWidth G.Roman "Stroke font"
		G.translate (G.Vector3 (x * fromIntegral w)
			y 0 :: G.Vector3 GLfloat)
		G.renderString G.Roman str

timerAction :: IO a -> IO ()
timerAction act = do
	_ <- act
	G.addTimerCallback 10 $ timerAction act

-- data InputType = XInput | End | Timer

closeField :: Field -> IO ()
closeField _ = return ()

waitField :: Field -> IO ()
waitField = const $ return ()

topleft, center :: Field -> IO ()
topleft = const $ return ()
center = const $ return ()

coordinates :: Field -> IO Coordinates
coordinates = return . fCoordinates

fieldSize :: Field -> IO (Double, Double)
fieldSize f = do
	w <- readIORef $ fWidth f
	h <- readIORef $ fHeight f
	return (fromIntegral w, fromIntegral h)

--------------------------------------------------------------------------------

forkField :: Field -> IO () -> IO ThreadId
forkField _f = forkIO

flushField :: Field -> Bool -> IO a -> IO a
flushField _f _real act = act

fieldColor :: Field -> Layer -> Color -> IO ()
fieldColor f _l clr = do
--	atomicModifyIORef_ (fActions f) ((++ [makeFieldColor clr]) . myInit)
	atomicModifyIORef_ (fBgcolor f) (clr :)
	atomicModifyIORef_ (fActions f) (Nothing :)

myInit [] = error "myInit failed"
myInit [x] = []
myInit (x : xs) = x : myInit xs

makeFieldColor clr = preservingMatrix $ do
	G.color $ colorToColor4 clr
	renderPrimitive Quads $ mapM_ vertex [
		G.Vertex2 (-1) (-1),
		G.Vertex2 (-1) 1,
		G.Vertex2 1 1,
		G.Vertex2 1 (-1) :: Vertex2 GLfloat ]

--------------------------------------------------------------------------------

setFieldSize :: Field -> Double -> Double -> IO ()
setFieldSize f w_ h_ = do
	let	w = round w_
		h = round h_
	writeIORef (fWidth f) w
	writeIORef (fHeight f) h
	currentWindow $= Just (fFieldWindow f)
	G.windowSize $= Size (fromIntegral w) (fromIntegral h)

drawLine :: Field -> Layer -> Double -> Color -> Position -> Position -> IO ()
drawLine f _ w c p q = do
	atomicModifyIORef_ (fActions f) (Just (makeLineAction f p q c w) :)
--	G.addTimerCallback 1 $ makeLineAction p q c
--	swapBuffers
	flush

makeLineAction :: Field -> Position -> Position -> Color -> Double -> IO ()
makeLineAction f p q c w = preservingMatrix $ do
	G.lineWidth $= fromRational (toRational w)
	G.color $ colorToColor4 c -- (G.Color4 1 0 0 0 :: G.Color4 GLfloat)
	pp <- positionToVertex3 f p
	qq <- positionToVertex3 f q
	renderPrimitive Lines $ mapM_ vertex [pp, qq]

colorToColor4 :: Color -> G.Color4 GLfloat
colorToColor4 (RGB r g b) = G.Color4
	(fromIntegral r / 255) (fromIntegral g / 255) (fromIntegral b / 255) 0
colorToColor4 _ = error "colorToColor4: not implemented"

makeQuads :: Field -> [Position] -> Color -> IO ()
makeQuads f ps c = do
	vs <- mapM (positionToVertex3 f) ps
	preservingMatrix $ do
		G.color $ colorToColor4 c
		renderPrimitive Quads $ mapM_ vertex vs

makeCharacterAction :: Field -> [Position] -> Color -> Color -> Double -> IO ()
makeCharacterAction f ps c lc lw = do
	vs <- mapM (positionToVertex3 f . posToPosition) $
		triangleToPositions $ toTriangles $ map positionToPos ps
	vs' <- mapM (positionToVertex3 f) ps
	preservingMatrix $ do
		G.color $ colorToColor4 c
		renderPrimitive Triangles $ mapM_ vertex vs
--			mapM_ vertex . positionToVertex3 f . posToPosition) $
--			triangleToPositions $
--			toTriangles $ map positionToPos ps
--		renderPrimitive Polygon $ mapM_ (vertex . positionToVertex3 f) ps
		G.lineWidth $= fromRational (toRational lw)
		G.color $ colorToColor4 lc
		renderPrimitive LineLoop $ mapM_ vertex vs'

type Pos = (Double, Double)
triangleToPositions :: [(Pos, Pos, Pos)] -> [Pos]
triangleToPositions [] = []
triangleToPositions ((a, b, c) : rest) = a : b : c : triangleToPositions rest

positionToPos :: Position -> Pos
positionToPos (Center x y) = (x, y)
positionToPos _ = error "positionToPos: not implemented"

posToPosition :: Pos -> Position
posToPosition (x, y) = Center x y

positionToVertex3 :: Field -> Position -> IO (Vertex2 GLfloat)
positionToVertex3 f (Center x y) = do
	w <- readIORef $ fWidth f
	h <- readIORef $ fHeight f
	return $ Vertex2
		(fromRational $ 2 * toRational x / fromIntegral w)
		(fromRational $ 2 * toRational y / fromIntegral h)
positionToVertex3 _ _ = error "positionToVertex3: not implemented"

writeString :: Field -> Layer -> String -> Double -> Color -> Position ->
	String -> IO ()
writeString f _ _fname size clr (Center x_ y_) str =
	atomicModifyIORef_ (fActions f) (Just action :)
	where
	action = preservingMatrix $ do
		h <- readIORef $ fHeight f
		w <- readIORef $ fWidth f
		let	size' = size / 15
			ratio = 3.5 * fromIntegral h -- 2000
			x_ratio = 2 * ratio / fromIntegral w
			y_ratio = 2 * ratio / fromIntegral h
			x = x_ratio * fromRational (toRational $ x_ / size')
			y = y_ratio * fromRational (toRational $ y_ / size')
--			s = 0.0005 * fromRational (toRational size')
			s = 1 / ratio * fromRational (toRational size')
		G.color $ colorToColor4 clr
		G.scale (s :: GLfloat) (s :: GLfloat) (s :: GLfloat)
		G.clearColor $= G.Color4 0 0 0 0
		G.translate (G.Vector3 x y 0 :: G.Vector3 GLfloat)
		G.renderString G.Roman str
writeString _ _ _ _ _ _ _ = error "writeString: not implemented"

drawImage :: Field -> Layer -> FilePath -> Position -> Double -> Double -> IO ()
drawImage _f _ _fp _pos _w _h = return ()

fillRectangle :: Field -> Layer -> Position -> Double -> Double -> Color -> IO ()
fillRectangle f _ p w h clr = do return ()
{-
	atomicModifyIORef_ (fActions f) (makeQuads f [
		Center 0 0, Center 0 100, Center 100 100, Center 100 0] clr])
-}

fillPolygon :: Field -> Layer -> [Position] -> Color -> Color -> Double -> IO ()
fillPolygon f _ ps clr lc lw =
	atomicModifyIORef_ (fActions f) (Just (makeCharacterAction f ps clr lc lw) :)

--------------------------------------------------------------------------------

drawCharacter :: Field -> Character -> Color -> Color -> [Position] -> Double -> IO ()
drawCharacter f _ fclr clr sh lw = writeIORef (fAction f) $
	makeCharacterAction f sh fclr clr lw

drawCharacterAndLine ::	Field -> Character -> Color -> Color -> [Position] ->
	Double -> Position -> Position -> IO ()
drawCharacterAndLine f _ fclr clr sh lw p q = writeIORef (fAction f) $ do
	makeLineAction f p q clr lw
	makeCharacterAction f sh fclr clr lw

clearCharacter :: Field -> IO ()
clearCharacter f = writeIORef (fAction f) $ return ()

--------------------------------------------------------------------------------

outputString :: Field -> String -> IO ()
outputString f = atomicModifyIORef_ (fString2 f) . (:)

oninputtext :: Field -> (String -> IO Bool) -> IO ()
oninputtext = writeIORef . fInputtext

onclick, onrelease :: Field -> (Int -> Double -> Double -> IO Bool) -> IO ()
onclick _ _ = return ()
onrelease _ _ = return ()

ondrag :: Field -> (Int -> Double -> Double -> IO ()) -> IO ()
ondrag _ _ = return ()

onmotion :: Field -> (Double -> Double -> IO ()) -> IO ()
onmotion _ _ = return ()

onkeypress :: Field -> (Char -> IO Bool) -> IO ()
onkeypress _ _ = return ()

ontimer :: Field -> Int -> IO Bool -> IO ()
ontimer _ _ _ = return ()

keyboardProc :: Field -> G.Key -> G.KeyState -> G.Modifiers -> G.Position -> IO ()
keyboardProc f (G.Char '\r') G.Down _ _ = do
	p <- readIORef $ fPrompt f
	str <- readIORef (fString f)
	atomicModifyIORef_ (fString2 f) (reverse str ++)
	writeIORef (fString f) [p]
	continue <- ($ drop (length p) $ concat str) =<< readIORef (fInputtext f)
	unless continue G.leaveMainLoop
keyboardProc f (G.Char '\b') G.Down _ _ = do
	p <- readIORef $ fPrompt f
	atomicModifyIORef_ (fString f) $ \s -> case s of
		[""] -> [""]
		[ss] | length ss <= length p -> s
		s -> case last s of
			"" -> init (init s) ++ [init $ last $ init s]
			_ -> init s ++ [init $ last s]
keyboardProc f (G.Char c) state _ _
	| state == G.Down = atomicModifyIORef_ (fString f) (`addToTail` c)
	| otherwise = return ()
keyboardProc _ _ _ _ _ = return ()

addToTail :: [String] -> Char -> [String]
addToTail strs c
	| null strs = error "bad"
	| length (last strs) < 50 = init strs ++ [last strs ++ [c]]
	| otherwise = strs ++ [[c]]
