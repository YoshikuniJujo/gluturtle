{-# LANGUAGE DoRec #-}

module Graphics.UI.GLUT.Turtle.Field(
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
	addCharacter
) where

import Control.Monad

import Graphics.UI.GLUT.Turtle.Triangles

import Graphics.UI.GLUT(
	createWindow, Vertex2(..), renderPrimitive, vertex, PrimitiveMode(..),
	preservingMatrix, GLfloat, swapBuffers, ($=), displayCallback,
	initialDisplayMode, initialWindowSize, Size(..),
	DisplayMode(..), flush
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

--------------------------------------------------------------------------------

data Coordinates = CoordTopLeft | CoordCenter

data Field = Field{
	fCoordinates :: Coordinates,

	fAction :: IORef (IO ()),
	fActions :: IORef [IO ()],

	fString :: IORef [String],
	fString2 :: IORef [String],

	fInputtext :: IORef (String -> IO Bool),

	fWidth :: Int,
	fHeight :: Int,

	fLayers :: IORef Layers
 }

addLayer :: Field -> IO Layer
addLayer = makeLayer . fLayers
addCharacter :: Field -> IO Character
addCharacter = makeCharacter . fLayers

--------------------------------------------------------------------------------

undoField :: Field -> IO ()
undoField f = atomicModifyIORef_ (fActions f) tail

openField :: String -> Int -> Int -> IO Field
openField name w h = do
	layers <- newLayers 0 (return ()) (return ()) (return ())
	action <- newIORef $ return ()
	actions <- newIORef []
	str <- newIORef [""]
	str2 <- newIORef []
	inputtext <- newIORef $ const $ return True

	initialDisplayMode $= [RGBMode, DoubleBuffered]
	initialWindowSize $= Size (fromIntegral w) (fromIntegral h)
	_ <- createWindow name
	displayCallback $= (sequence_ =<< readIORef actions)
	G.addTimerCallback 10 (timerAction $ do
		G.clearColor $= G.Color4 0 0 0 0
		G.clear [G.ColorBuffer]
		sequence_ . reverse =<< readIORef actions
		join $ readIORef action
		G.lineWidth $= 1.0
		ss1 <- readIORef str
		ss2 <- readIORef str2
		zipWithM_ (printString (-2.8)) [-1800, -1600 .. 0] (reverse ss1 ++ ss2)
{-
		printString (-2.5) (-1800) . concat =<< readIORef str
		zipWithM_ (printString (-2.5)) [-1600, -1400 .. 0] =<< readIORef str2
-}
		swapBuffers)
	G.reshapeCallback $= Just (\size -> G.viewport $= (G.Position 0 0, size))
	let f = Field{
		fCoordinates = CoordCenter,
		fLayers = layers,
		fAction = action,
		fActions = actions,
		fString = str,
		fString2 = str2,
		fWidth = w,
		fHeight = h,
		fInputtext = inputtext
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
fieldSize = const $ return (0, 0)

--------------------------------------------------------------------------------

forkField :: Field -> IO () -> IO ThreadId
forkField _f = forkIO

flushField :: Field -> Bool -> IO a -> IO a
flushField _f _real act = act

fieldColor :: Field -> Layer -> Color -> IO ()
fieldColor _f _l _clr = return ()

--------------------------------------------------------------------------------

drawLine :: Field -> Layer -> Double -> Color -> Position -> Position -> IO ()
drawLine f _ w c p q = do
	atomicModifyIORef_ (fActions f) (makeLineAction f p q c w :)
--	G.addTimerCallback 1 $ makeLineAction p q c
--	swapBuffers
	flush

makeLineAction :: Field -> Position -> Position -> Color -> Double -> IO ()
makeLineAction f p q c w = preservingMatrix $ do
	G.lineWidth $= fromRational (toRational w)
	G.color $ colorToColor4 c -- (G.Color4 1 0 0 0 :: G.Color4 GLfloat)
	renderPrimitive Lines $ mapM_ vertex [
		positionToVertex3 f p,
		positionToVertex3 f q ]

colorToColor4 :: Color -> G.Color4 GLfloat
colorToColor4 (RGB r g b) = G.Color4
	(fromIntegral r / 255) (fromIntegral g / 255) (fromIntegral b / 255) 0
colorToColor4 _ = error "colorToColor4: not implemented"

makeCharacterAction :: Field -> [Position] -> Color -> Color -> Double -> IO ()
makeCharacterAction f ps c lc lw =
	preservingMatrix $ do
		G.color $ colorToColor4 c
		renderPrimitive Triangles $
			mapM_ (vertex . positionToVertex3 f . posToPosition) $
			triangleToPositions $
			toTriangles $ map positionToPos ps
--		renderPrimitive Polygon $ mapM_ (vertex . positionToVertex3 f) ps
		G.lineWidth $= fromRational (toRational lw)
		G.color $ colorToColor4 lc
		renderPrimitive LineLoop $ mapM_ (vertex . positionToVertex3 f) ps

type Pos = (Double, Double)
triangleToPositions :: [(Pos, Pos, Pos)] -> [Pos]
triangleToPositions [] = []
triangleToPositions ((a, b, c) : rest) = a : b : c : triangleToPositions rest

positionToPos :: Position -> Pos
positionToPos (Center x y) = (x, y)
positionToPos _ = error "positionToPos: not implemented"

posToPosition :: Pos -> Position
posToPosition (x, y) = Center x y

positionToVertex3 :: Field -> Position -> Vertex2 GLfloat
positionToVertex3 f (Center x y) =
	Vertex2 (fromRational $ toRational x / fromIntegral (fWidth f))
		(fromRational $ toRational y / fromIntegral (fHeight f))
positionToVertex3 _ _ = error "positionToVertex3: not implemented"

writeString :: Field -> Layer -> String -> Double -> Color -> Position ->
	String -> IO ()
writeString f _ _fname size clr (Center x_ y_) str =
	atomicModifyIORef_ (fActions f) (action :)
	where
	action = preservingMatrix $ do
		let	size' = size / 15
			ratio = 7 * fromIntegral (fHeight f) -- 2000
			x_ratio = ratio / fromIntegral (fWidth f)
			y_ratio = ratio / fromIntegral (fHeight f)
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
fillRectangle _f _ _p _w _h _clr = return ()

fillPolygon :: Field -> Layer -> [Position] -> Color -> Color -> Double -> IO ()
fillPolygon f _ ps clr lc lw =
	atomicModifyIORef_ (fActions f) (makeCharacterAction f ps clr lc lw :)

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
	str <- readIORef (fString f)
	atomicModifyIORef_ (fString2 f) (reverse str ++)
	writeIORef (fString f) [""]
	continue <- ($ concat str) =<< readIORef (fInputtext f)
	unless continue G.leaveMainLoop
keyboardProc f (G.Char '\b') G.Down _ _ =
	atomicModifyIORef_ (fString f) $ \s -> case s of
		[""] -> [""]
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
