module Graphics.UI.GLUT.Turtle.Field(

	-- * types and classes
	Field,
	Coordinates(..),

	-- * basic functions
	initialize,
	openField,
	openConsole,
	setConsole,
	closeField,
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
	undoField,

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

	prompt
) where

import Control.Monad

import Graphics.UI.GLUT.Turtle.Triangles

import qualified Graphics.UI.GLUT.Turtle.GLUTools as G
import Graphics.UI.GLUT.Turtle.GLUTools(
	($=), initialize, createWindow, loop,
	displayAction)
import Text.XML.YJSVG(Position(..), Color(..))

import Control.Concurrent(ThreadId, forkIO, readChan)
import Data.IORef(IORef, newIORef, readIORef, writeIORef)
import Data.IORef.Tools(atomicModifyIORef_)
import Data.Maybe

import Graphics.UI.GLUT.Turtle.Console

--------------------------------------------------------------------------------

prompt :: Field -> String -> IO ()
prompt f p = do
	mc <- getConsole f
	case mc of
		Just c -> do
			writeIORef (cPrompt c) p
			atomicModifyIORef_ (cCommand c)
				(\ls -> init ls ++ [p ++ last ls])
		_ -> return ()

data Coordinates = CoordTopLeft | CoordCenter

data Field = Field{
	fConsole :: IORef (Maybe Console),

	fFieldWindow :: G.Window,
	fSize :: IORef (Int, Int),
	fCoordinates :: IORef Coordinates,
	fBgcolor :: IORef [Color],

	fAction :: IORef (IO ()),
	fActions :: IORef [Maybe (IO ())],

	fChanged :: IORef Int,

	fInputtext :: IORef (String -> IO Bool),
	fOnclick :: IORef (Int -> Double -> Double -> IO Bool)
 }

--------------------------------------------------------------------------------

openField :: String -> Int -> Int -> IO Field
openField name w h = do
	fsize <- newIORef (w, h)
	fcoord <- newIORef CoordCenter
	fbgcolor <- newIORef [RGB 255 255 255]

	faction <- newIORef $ return ()
	factions <- newIORef []

	fchanged <- newIORef 0
	finputtext <- newIORef $ const $ return True
	fclick <- newIORef (\_ _ _ -> return True)

	ffield <- createWindow name w h

	let	act = do
			G.currentWindow $= Just ffield
			actwt
		actwt = do
			G.Size w' h' <- G.get G.windowSize
			writeIORef fsize $ (fromIntegral w', fromIntegral h')
			G.clearColor $= G.Color4 0 0 0 0
			G.clear [G.ColorBuffer]
			makeFieldColor . head =<< readIORef fbgcolor
			sequence_ . reverse . catMaybes =<< readIORef factions
			join $ readIORef faction
			G.swapBuffers
	displayAction fchanged act
	G.reshapeCallback $= Just (\size -> G.viewport $= (G.Position 0 0, size))

	fconsole <- newIORef Nothing
	let f = Field{
		fConsole = fconsole,

		fFieldWindow = ffield,
		fSize = fsize,
		fCoordinates = fcoord,
		fBgcolor = fbgcolor,

		fAction = faction,
		fActions = factions,

		fChanged = fchanged,

		fInputtext = finputtext,
		fOnclick = fclick
	 }
	G.keyboardMouseCallback $= Just (processKeyboardMouse f)

	return f

setConsole :: Field -> Console -> IO ()
setConsole f console = do
	loop (cChanChanged console) $ do
		cmd <- readChan $ cChan console
		continue <- readIORef (fInputtext f) >>= ($ cmd)
		unless continue G.leaveMainLoop
	writeIORef (fConsole f) $ Just console

processKeyboardMouse :: Field -> G.Key -> G.KeyState -> G.Modifiers -> G.Position -> IO ()
processKeyboardMouse f (G.Char c) ks m p = do
	mc <- getConsole f
	case mc of
		Just con -> do
			processKeyboard con c ks m p
			atomicModifyIORef_ (fChanged f) (+ 1)
		Nothing -> return ()
processKeyboardMouse f (G.MouseButton mb) G.Down _m (G.Position x_ y_) = do
	coord <- readIORef (fCoordinates f)
	continue <- case coord of
		CoordCenter -> do
			(w, h) <- fieldSize f
			let	x = fromIntegral x_ - (w / 2)
				y = (h / 2) - fromIntegral y_
			readIORef (fOnclick f) >>= (\fun -> fun (buttonToInt mb) x y)
		CoordTopLeft -> do
			let	(x, y) = (fromIntegral x_, fromIntegral y_)
			readIORef (fOnclick f) >>= (\fun -> fun (buttonToInt mb) x y)
	unless continue G.leaveMainLoop
processKeyboardMouse _f (G.MouseButton _mb) G.Up _m _p = do
	return ()
processKeyboardMouse _f (G.SpecialKey _sk) _ks _m _p = do
	return ()

buttonToInt :: G.MouseButton -> Int
buttonToInt G.LeftButton = 1
buttonToInt G.MiddleButton = 2
buttonToInt G.RightButton = 3
buttonToInt G.WheelUp = 4
buttonToInt G.WheelDown = 5
buttonToInt (G.AdditionalButton n) = n

undoField :: Field -> IO ()
undoField f = do
	a : _ <- readIORef $ fActions f
	when (isNothing a) $ atomicModifyIORef_ (fBgcolor f) tail
	atomicModifyIORef_ (fActions f) tail

closeField :: Field -> IO ()
closeField _ = G.leaveMainLoop

topleft, center :: Field -> IO ()
topleft = flip writeIORef CoordTopLeft . fCoordinates
center = flip writeIORef CoordCenter . fCoordinates

coordinates :: Field -> IO Coordinates
coordinates = readIORef . fCoordinates

fieldSize :: Field -> IO (Double, Double)
fieldSize f = do
	(w, h) <- readIORef $ fSize f
	return (fromIntegral w, fromIntegral h)

--------------------------------------------------------------------------------

forkField :: Field -> IO () -> IO ThreadId
forkField _f = forkIO

flushField :: Field -> Bool -> IO a -> IO a
flushField _f _real act = act

fieldColor :: Field -> Color -> IO ()
fieldColor f clr = do
	atomicModifyIORef_ (fBgcolor f) (clr :)
	atomicModifyIORef_ (fActions f) (Nothing :)

makeFieldColor :: Color -> IO ()
makeFieldColor clr = G.preservingMatrix $ do
	G.color $ colorToColor4 clr
	G.renderPrimitive G.Quads $ mapM_ G.vertex [
		G.Vertex2 (-1) (-1),
		G.Vertex2 (-1) 1,
		G.Vertex2 1 1,
		G.Vertex2 1 (-1) :: G.Vertex2 G.GLfloat ]

--------------------------------------------------------------------------------

setFieldSize :: Field -> Double -> Double -> IO ()
setFieldSize f w_ h_ = do
	let	w = round w_
		h = round h_
	writeIORef (fSize f) (w, h)
	G.currentWindow $= Just (fFieldWindow f)
	G.windowSize $= G.Size (fromIntegral w) (fromIntegral h)

drawLine :: Field -> Double -> Color -> Position -> Position -> IO ()
drawLine f w c p q = do
	atomicModifyIORef_ (fActions f) (Just (makeLineAction f p q c w) :)
	atomicModifyIORef_ (fChanged f) (+ 1)
	G.flush

makeLineAction :: Field -> Position -> Position -> Color -> Double -> IO ()
makeLineAction f p q c w = G.preservingMatrix $ do
	G.lineWidth $= fromRational (toRational w)
	G.color $ colorToColor4 c
	pp <- positionToVertex3 f p
	qq <- positionToVertex3 f q
	G.renderPrimitive G.Lines $ mapM_ G.vertex [pp, qq]

colorToColor4 :: Color -> G.Color4 G.GLfloat
colorToColor4 (RGB r g b) = G.Color4
	(fromIntegral r / 255) (fromIntegral g / 255) (fromIntegral b / 255) 0
colorToColor4 _ = error "colorToColor4: not implemented"

makeCharacterAction :: Field -> [Position] -> Color -> Color -> Double -> IO ()
makeCharacterAction f ps c lc lw = do
	ps' <- mapM (positionToPos f) ps
	vs <- mapM (positionToVertex3 f . posToPosition) $
		triangleToPositions $ toTriangles ps'
	vs' <- mapM (positionToVertex3 f) ps
	G.preservingMatrix $ do
		G.color $ colorToColor4 c
		G.renderPrimitive G.Triangles $ mapM_ G.vertex vs
		G.lineWidth $= fromRational (toRational lw)
		G.color $ colorToColor4 lc
		G.renderPrimitive G.LineLoop $ mapM_ G.vertex vs'

type Pos = (Double, Double)
triangleToPositions :: [(Pos, Pos, Pos)] -> [Pos]
triangleToPositions [] = []
triangleToPositions ((a, b, c) : rest) = a : b : c : triangleToPositions rest

positionToPos :: Field -> Position -> IO Pos
positionToPos _ (Center x y) = return (x, y)
positionToPos f (TopLeft x y) = do
	(w, h) <- fieldSize f
	return (x - w / 2, h / 2 - y)

posToPosition :: Pos -> Position
posToPosition (x, y) = Center x y

positionToVertex3 :: Field -> Position -> IO (G.Vertex2 G.GLfloat)
positionToVertex3 f (Center x y) = do
	(w, h) <- readIORef $ fSize f
	return $ G.Vertex2
		(fromRational $ 2 * toRational x / fromIntegral w)
		(fromRational $ 2 * toRational y / fromIntegral h)
positionToVertex3 f (TopLeft x y) = do
	(w, h) <- readIORef $ fSize f
	let	x' = 2 * toRational x / fromIntegral w - 1
		y' = 1 - 2 * toRational y / fromIntegral h
	return $ G.Vertex2 (fromRational x') (fromRational y')

writeString :: Field -> String -> Double -> Color -> Position ->
	String -> IO ()
writeString f _fname size clr (Center x_ y_) str =
	atomicModifyIORef_ (fActions f) (Just action :)
	where
	action = G.preservingMatrix $ do
		(w, h) <- readIORef $ fSize f
		let	size' = size / 15
			ratio = 3.5 * fromIntegral h
			x_ratio = 2 * ratio / fromIntegral w
			y_ratio = 2 * ratio / fromIntegral h
			x = x_ratio * fromRational (toRational $ x_ / size')
			y = y_ratio * fromRational (toRational $ y_ / size')
			s = 1 / ratio * fromRational (toRational size')
		G.color $ colorToColor4 clr
		G.scale (s :: G.GLfloat) (s :: G.GLfloat) (s :: G.GLfloat)
		G.clearColor $= G.Color4 0 0 0 0
		G.translate (G.Vector3 x y 0 :: G.Vector3 G.GLfloat)
		G.renderString G.Roman str
writeString _ _ _ _ _ _ = error "writeString: not implemented"

drawImage :: Field -> FilePath -> Position -> Double -> Double -> IO ()
drawImage _f _fp _pos _w _h = return ()

fillRectangle :: Field -> Position -> Double -> Double -> Color -> IO ()
fillRectangle _f _p _w _h _clr = return ()

fillPolygon :: Field -> [Position] -> Color -> Color -> Double -> IO ()
fillPolygon f ps clr lc lw = do
	atomicModifyIORef_ (fActions f) (Just (makeCharacterAction f ps clr lc lw) :)
	atomicModifyIORef_ (fChanged f) (+ 1)

--------------------------------------------------------------------------------

drawCharacter :: Field -> Color -> Color -> [Position] -> Double -> IO ()
drawCharacter f fclr clr sh lw = do
	makeCharacterAction f sh fclr clr lw
	writeIORef (fAction f) $
		makeCharacterAction f sh fclr clr lw
	atomicModifyIORef_ (fChanged f) (+ 1)

drawCharacterAndLine ::	Field -> Color -> Color -> [Position] ->
	Double -> Position -> Position -> IO ()
drawCharacterAndLine f fclr clr sh lw p q = do
	writeIORef (fAction f) $ do
		makeLineAction f p q clr lw
		makeCharacterAction f sh fclr clr lw
	atomicModifyIORef_ (fChanged f) (+ 1)

clearCharacter :: Field -> IO ()
clearCharacter f = writeIORef (fAction f) $ return ()

--------------------------------------------------------------------------------

getConsole :: Field -> IO (Maybe Console)
getConsole = readIORef . fConsole

outputString :: Field -> String -> IO ()
outputString f str = do
	mc <- getConsole f
	case mc of
		Just c -> atomicModifyIORef_ (cHistory c) (str :)
		_ -> return ()

oninputtext :: Field -> (String -> IO Bool) -> IO ()
oninputtext = writeIORef . fInputtext

onclick, onrelease :: Field -> (Int -> Double -> Double -> IO Bool) -> IO ()
onclick f act = writeIORef (fOnclick f) act
onrelease _ _ = return ()

ondrag :: Field -> (Int -> Double -> Double -> IO ()) -> IO ()
ondrag _ _ = return ()

onmotion :: Field -> (Double -> Double -> IO ()) -> IO ()
onmotion _ _ = return ()

onkeypress :: Field -> (Char -> IO Bool) -> IO ()
onkeypress _ _ = return ()

ontimer :: Field -> Int -> IO Bool -> IO ()
ontimer _ _ _ = return ()
