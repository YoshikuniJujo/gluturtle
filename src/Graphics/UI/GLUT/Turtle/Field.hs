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

import Graphics.UI.GLUT(
	createWindow, Vertex2(..), renderPrimitive, vertex, PrimitiveMode(..),
	preservingMatrix, GLfloat, swapBuffers, ($=), displayCallback,
	initialDisplayMode, initialWindowSize, Size(..),
	DisplayMode(..), flush, Vertex3(..)
 )
import qualified Graphics.UI.GLUT as G

import Graphics.UI.GLUT.Turtle.Layers(
	Layers, Layer, Character, newLayers, redrawLayers,
	makeLayer, background, addDraw, undoLayer, clearLayer,
	makeCharacter, character)
import Text.XML.YJSVG(Position(..), Color(..))

import Control.Monad(when, unless, forever, replicateM, forM_, join)
import Control.Monad.Tools(doWhile_, doWhile)
import Control.Arrow((***))
import Control.Concurrent(
	ThreadId, forkIO, killThread, threadDelay,
	Chan, newChan, readChan, writeChan)
import Data.IORef(IORef, newIORef, readIORef, writeIORef)
import Data.IORef.Tools(atomicModifyIORef_)
import Data.Maybe(fromMaybe)
import Data.List(delete)
import Data.Convertible(convert)
import Data.Function.Tools(const2, const3)

--------------------------------------------------------------------------------

data Coordinates = CoordTopLeft | CoordCenter

data Field = Field{
	fCoordinates :: Coordinates,

	fAction :: IORef (IO ()),
	fActions :: IORef [IO ()],

	fLayers :: IORef Layers
 }

addLayer = makeLayer . fLayers
addCharacter = makeCharacter . fLayers

--------------------------------------------------------------------------------

undoField :: Field -> IO ()
undoField f = atomicModifyIORef_ (fActions f) tail

openField :: IO Field
openField = do
	layers <- newLayers 0 (return ()) (return ()) (return ())
	action <- newIORef $ return ()
	actions <- newIORef []

	initialDisplayMode $= [RGBMode, DoubleBuffered]
	initialWindowSize $= Size 640 480
	createWindow "field"
	displayCallback $= (sequence_ =<< readIORef actions) -- testAction
	G.addTimerCallback 10 (timerAction $ do
		G.clearColor $= G.Color4 0 0 0 0
		G.clear [G.ColorBuffer]
		sequence_ =<< readIORef actions
		join $ readIORef action
		swapBuffers)
	G.reshapeCallback $= Just (\size -> G.viewport $= (G.Position 0 0, size))
	print "main loop go"
	print "main loop"
	return Field{
		fCoordinates = CoordCenter,
		fLayers = layers,
		fAction = action,
		fActions = actions
	 }

timerAction act = do
	act
	G.addTimerCallback 10 $ timerAction act

data InputType = XInput | End | Timer

waitInput :: Field -> IO (Chan ())
waitInput f = newChan

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
forkField f act = do
	tid <- forkIO act
	return tid

flushField :: Field -> Bool -> IO a -> IO a
flushField f real act = act

fieldColor :: Field -> Layer -> Color -> IO ()
fieldColor f l clr = return ()

--------------------------------------------------------------------------------

drawLayer f l drw = addDraw l (drw, drw)

drawLine :: Field -> Layer -> Double -> Color -> Position -> Position -> IO ()
drawLine f l w c p q = do
	atomicModifyIORef_ (fActions f) (makeLineAction p q c :)
--	G.addTimerCallback 1 $ makeLineAction p q c
--	swapBuffers
	flush
{- do
	preservingMatrix $ do
		renderPrimitive Quads $ mapM_ vertex [
			Vertex3 0.10 0.10 0.0,
			Vertex3 (-0.10) 0.10 0.0,
			Vertex3 (-0.10) (-0.10) 0.0,
			Vertex3 0.10 (-0.10) 0.0 :: Vertex3 GLfloat
		 ]
	swapBuffers -}

testAction = do
	G.loadIdentity
	preservingMatrix $ do
		renderPrimitive Lines $ mapM_ vertex [
			Vertex3 0.10 0.10 0.0,
			Vertex3 (-0.10) 0.10 0.0,
			Vertex3 (-0.10) (-0.10) 0.0,
			Vertex3 0.10 (-0.10) 0.0 :: Vertex3 GLfloat
		 ]
--	swapBuffers

makeLineAction :: Position -> Position -> Color -> IO ()
makeLineAction p q c = do
	preservingMatrix $ do
		G.color $ colorToColor4 c -- (G.Color4 1 0 0 0 :: G.Color4 GLfloat)
		renderPrimitive Lines $ mapM_ vertex [
			positionToVertex3 p,
			positionToVertex3 q ]
--	swapBuffers

colorToColor4 :: Color -> G.Color4 GLfloat
colorToColor4 (RGB r g b) = G.Color4
	(fromIntegral r / 255) (fromIntegral g / 255) (fromIntegral b / 255) 0

makeCharacterAction :: [Position] -> IO ()
makeCharacterAction ps =
	preservingMatrix $ do
		renderPrimitive Polygon $ mapM_ (vertex . positionToVertex3) ps

positionToVertex3 :: Position -> Vertex2 GLfloat
positionToVertex3 (Center x y) =
	Vertex2 (fromRational $ toRational x / 300)
		(fromRational $ toRational y / 300)

writeString :: Field -> Layer -> String -> Double -> Color -> Position ->
	String -> IO ()
writeString f l fname size clr pos str = return ()

drawImage :: Field -> Layer -> FilePath -> Position -> Double -> Double -> IO ()
drawImage f l fp pos w h = return ()

fillRectangle :: Field -> Layer -> Position -> Double -> Double -> Color -> IO ()
fillRectangle f l p w h clr = return ()

fillPolygon :: Field -> Layer -> [Position] -> Color -> Color -> Double -> IO ()
fillPolygon f l ps clr lc lw = return ()

--------------------------------------------------------------------------------

drawCharacter :: Field -> Character -> Color -> Color -> [Position] -> Double -> IO ()
drawCharacter f ch fc c ps lw = return ()

drawCharacterAndLine ::	Field -> Character -> Color -> Color -> [Position] ->
	Double -> Position -> Position -> IO ()
drawCharacterAndLine f ch fclr clr sh lw p q =
	writeIORef (fAction f) $ do
		makeLineAction p q clr
		makeCharacterAction sh

clearCharacter :: Character -> IO ()
clearCharacter ch = character ch $ return ()

--------------------------------------------------------------------------------

oninputtext :: Field -> (String -> IO Bool) -> IO ()
oninputtext _ _ = return ()

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
ontimer f t fun = return ()
