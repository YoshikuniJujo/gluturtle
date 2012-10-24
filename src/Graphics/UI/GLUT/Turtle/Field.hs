module Graphics.UI.GLUT.Turtle.Field(

	-- * types and classes
	Field,
	Console,
	Coordinates(..),

	-- * basic functions
	initialize,
	openField,
	closeField,
	topleft,
	center,
	coordinates,
	fieldSize,
	setFieldSize,

	-- * about Console
	openConsole,
	setConsole,
	consolePrompt,
	consoleOutput,

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

	-- * event driven
	oncommand,
	onclick,
	onrelease,
	ondrag,
	onmotion,
	onkeypress,
	ontimer
) where

import Graphics.UI.GLUT.Turtle.Console(
	Console, openConsole, consolePrompt, consoleOutput,
	consoleKeyboard, consoleCommand)
import Graphics.UI.GLUT.Turtle.GLUTools(
	Window, Key(..), KeyState(..), Modifiers, Vertex3(..), Color4(..),
	GLfloat,
	initialize, createWindow, loop, displayAction, keyboardMouseCallback,
	currentWindow, swapBuffers, leaveUnless,
	windowColor, windowSize, setWindowSize,
	glDrawLine, drawPolygon, glWriteString)
import Text.XML.YJSVG(Position(..), Color(..))

import Control.Arrow((***))
import Control.Applicative((<$>))
import Control.Monad(when, join)
import Control.Concurrent(ThreadId, forkIO)
import Data.Maybe(isNothing, catMaybes)
import Data.IORef(IORef, newIORef, readIORef, writeIORef, atomicModifyIORef)
import Data.IORef.Tools(atomicModifyIORef_)

--------------------------------------------------------------------------------

data Coordinates = CoordTopLeft | CoordCenter

data Field = Field{
	fWindow :: Window,
	fSize :: IORef (Int, Int),
	fCoordinates :: IORef Coordinates,
	fBgcolor :: IORef [Color],

	fUpdate :: IORef Int,
	fAction :: IORef (IO ()),
	fActions :: IORef [Maybe (IO ())],

	fConsole :: IORef (Maybe Console),
	fOncommand :: IORef (String -> IO Bool),
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
	fupdate <- newIORef 0
	foncommand <- newIORef $ const $ return True
	fclick <- newIORef (\_ _ _ -> return True)
	fwindow <- createWindow name w h
	fconsole <- newIORef Nothing
	let	field = Field{
			fConsole = fconsole,
			fWindow = fwindow,
			fSize = fsize,
			fCoordinates = fcoord,
			fBgcolor = fbgcolor,
			fAction = faction,
			fActions = factions,
			fUpdate = fupdate,
			fOncommand = foncommand,
			fOnclick = fclick }
	keyboardMouseCallback $ procKboardMouse field
	displayAction fupdate $ do
		currentWindow fwindow
		writeIORef fsize =<< windowSize
		windowColor . colorToColor4 . head =<< readIORef fbgcolor
		sequence_ . reverse . catMaybes =<< readIORef factions
		join $ readIORef faction
		swapBuffers
	return field

setConsole :: Field -> Console -> IO ()
setConsole f c = (writeIORef (fConsole f) (Just c) >>) $ loop $ do
	mcmd <- consoleCommand c
	case mcmd of
		Just cmd -> readIORef (fOncommand f) >>= ($ cmd) >>= leaveUnless
		_ -> return ()

procKboardMouse ::
	Field -> Key -> KeyState -> Modifiers -> (Double, Double) -> IO ()
procKboardMouse Field{fConsole = con} (Char chr) ks m _ = readIORef con >>=
	maybe (return ()) (\c -> consoleKeyboard c chr ks m)
procKboardMouse field (MouseButton mb) Down _ (x, y) = do
	coord <- readIORef $ fCoordinates field
	fun <- readIORef $ fOnclick field
	continue <- case coord of
		CoordCenter -> do
			(w, h) <- fieldSize field
			fun mb (x - w / 2) (h / 2 - y)
		CoordTopLeft -> fun mb x y
	leaveUnless continue
procKboardMouse _f (MouseButton _mb) Up _m _p = return ()
procKboardMouse _f (SpecialKey _sk) _ks _m _p = return ()

undoField :: Field -> IO ()
undoField f = do
	ret <- atomicModifyIORef (fActions f) (\(h : t) -> (t, h))
	when (isNothing ret) $ atomicModifyIORef_ (fBgcolor f) tail

closeField :: Field -> IO ()
closeField _ = return ()

topleft, center :: Field -> IO ()
topleft = flip writeIORef CoordTopLeft . fCoordinates
center = flip writeIORef CoordCenter . fCoordinates

coordinates :: Field -> IO Coordinates
coordinates = readIORef . fCoordinates

fieldSize :: Field -> IO (Double, Double)
fieldSize f = (fromIntegral *** fromIntegral) <$> readIORef (fSize f)

--------------------------------------------------------------------------------

forkField :: Field -> IO () -> IO ThreadId
forkField _f = forkIO

flushField :: Field -> Bool -> IO a -> IO a
flushField _f _real act = act

fieldColor :: Field -> Color -> IO ()
fieldColor f clr = do
	atomicModifyIORef_ (fBgcolor f) (clr :)
	atomicModifyIORef_ (fActions f) (Nothing :)

--------------------------------------------------------------------------------

setFieldSize :: Field -> Double -> Double -> IO ()
setFieldSize f w_ h_ = do
	let	(w, h) = (round *** round) (w_, h_)
	writeIORef (fSize f) (w, h)
	currentWindow $ fWindow f
	setWindowSize w h

drawLine :: Field -> Double -> Color -> Position -> Position -> IO ()
drawLine f w c p q = do
	atomicModifyIORef_ (fUpdate f) (+ 1)
	atomicModifyIORef_ (fActions f) (Just (makeLineAction f w c p q) :)

makePolygonAction :: Field -> [Position] -> Color -> Color -> Double -> IO ()
makePolygonAction f ps c lc lw = do
	vs' <- mapM (positionToVertex3 f) ps
	drawPolygon vs' (colorToColor4 c) (colorToColor4 lc) (doubleToGLfloat lw)

makeLineAction :: Field -> Double -> Color -> Position -> Position -> IO ()
makeLineAction f w c p_ q_ = do
	[p, q] <- mapM (positionToVertex3 f) [p_, q_]
	glDrawLine (colorToColor4 c) (doubleToGLfloat w) p q


doubleToGLfloat :: Double -> GLfloat
doubleToGLfloat = fromRational . toRational
positionToVertex3 :: Field -> Position -> IO (Vertex3 GLfloat)
positionToVertex3 f (Center x y) = do
	(w, h) <- readIORef $ fSize f
	return $ Vertex3 (doubleToGLfloat $ 2 * x / fromIntegral w)
		(doubleToGLfloat $ 2 * y / fromIntegral h) 0
positionToVertex3 f (TopLeft x y) = do
	(w, h) <- readIORef $ fSize f
	return $ Vertex3 (doubleToGLfloat $ 2 * x / fromIntegral w - 1)
		(doubleToGLfloat $ 1 - 2 * y / fromIntegral h) 0

colorToColor4 :: Color -> Color4 GLfloat
colorToColor4 (RGB r g b) = Color4
	(fromIntegral r / 255) (fromIntegral g / 255) (fromIntegral b / 255) 0
colorToColor4 _ = error "colorToColor4: not implemented"

writeString :: Field -> String -> Double -> Color -> Position ->
	String -> IO ()
writeString f _fname size clr (Center x_ y_) str = do
	(w, h) <- readIORef $ fSize f
	let	ratio = 3.5 * fromIntegral h
		size' = size / 15
		x_ratio = 2 * ratio / fromIntegral w
		y_ratio = 2 * ratio / fromIntegral h
		x = x_ratio * (doubleToGLfloat $ x_ / size')
		y = y_ratio * (doubleToGLfloat $ y_ / size')
		s = 1 / ratio * (doubleToGLfloat size')
		action = glWriteString s (colorToColor4 clr) x y str
	atomicModifyIORef_ (fActions f) (Just action :)
writeString _ _ _ _ _ _ = error "writeString: not implemented"

drawImage :: Field -> FilePath -> Position -> Double -> Double -> IO ()
drawImage _f _fp _pos _w _h = return ()

fillRectangle :: Field -> Position -> Double -> Double -> Color -> IO ()
fillRectangle _f _p _w _h _clr = return ()

fillPolygon :: Field -> [Position] -> Color -> Color -> Double -> IO ()
fillPolygon f ps clr lc lw = do
	atomicModifyIORef_ (fActions f) (Just (makePolygonAction f ps clr lc lw) :)
	atomicModifyIORef_ (fUpdate f) (+ 1)

--------------------------------------------------------------------------------

drawCharacter :: Field -> Color -> Color -> [Position] -> Double -> IO ()
drawCharacter f fclr clr sh lw = do
	makePolygonAction f sh fclr clr lw
	writeIORef (fAction f) $
		makePolygonAction f sh fclr clr lw
	atomicModifyIORef_ (fUpdate f) (+ 1)

drawCharacterAndLine ::	Field -> Color -> Color -> [Position] ->
	Double -> Position -> Position -> IO ()
drawCharacterAndLine f fclr clr sh lw p q = do
	writeIORef (fAction f) $ do
		makeLineAction f lw clr p q
		makePolygonAction f sh fclr clr lw
	atomicModifyIORef_ (fUpdate f) (+ 1)

clearCharacter :: Field -> IO ()
clearCharacter f = writeIORef (fAction f) $ return ()

--------------------------------------------------------------------------------

oncommand :: Field -> (String -> IO Bool) -> IO ()
oncommand = writeIORef . fOncommand

onclick, onrelease :: Field -> (Int -> Double -> Double -> IO Bool) -> IO ()
onclick = writeIORef . fOnclick
onrelease _ _ = return ()

ondrag :: Field -> (Int -> Double -> Double -> IO ()) -> IO ()
ondrag _ _ = return ()

onmotion :: Field -> (Double -> Double -> IO ()) -> IO ()
onmotion _ _ = return ()

onkeypress :: Field -> (Char -> IO Bool) -> IO ()
onkeypress _ _ = return ()

ontimer :: Field -> Int -> IO Bool -> IO ()
ontimer _ _ _ = return ()
