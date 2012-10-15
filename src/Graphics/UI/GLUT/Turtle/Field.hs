module Graphics.UI.GLUT.Turtle.Field(

	-- * types and classes
	Field,
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

import Graphics.UI.GLUT(
	createWindow, Vertex2(..), renderPrimitive, vertex, PrimitiveMode(..),
	preservingMatrix, GLfloat, swapBuffers, ($=), displayCallback,
	initialDisplayMode, initialWindowSize, Size(..),
	DisplayMode(..), flush, currentWindow, Window
 )
import qualified Graphics.UI.GLUT as G
import Text.XML.YJSVG(Position(..), Color(..))

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

prompt :: Field -> String -> IO ()
prompt f p = do
	writeIORef (fPrompt f) p
	atomicModifyIORef_ (fString f) (\ls -> init ls ++ [p ++ last ls])

data Coordinates = CoordTopLeft | CoordCenter

data Field = Field{
	fChanged :: IORef Int,
	fAct :: IO (),
	fCoordinates :: IORef Coordinates,

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

	fBusy :: IORef Bool,

	fOnclick :: IORef (Int -> Double -> Double -> IO Bool)
 }

--------------------------------------------------------------------------------

openField :: String -> Int -> Int -> IO Field
openField name w h = do
	click <- newIORef (\_ _ _ -> return True)
	fc <- newIORef 0
	fb <- newIORef False
	fw <- newIORef w
	fh <- newIORef h
	bgc <- newIORef [RGB 255 255 255]
	action <- newIORef $ return ()
	actions <- newIORef []
	str <- newIORef [""]
	str2 <- newIORef []
	inputtext <- newIORef $ const $ return True

	prmpt <- newIORef ""

	initialDisplayMode $= [RGBMode, DoubleBuffered]
	initialWindowSize $= Size (fromIntegral w) (fromIntegral h)
	wt <- createWindow name
	wc <- createWindow "console"
	let	act = do
			change <- readIORef fc
			when (change > 0) $ do
				currentWindow $= Just wc
				actwc
				currentWindow $= Just wt
				actwt
				atomicModifyIORef_ fc (subtract 1)
		actwt = do
			Size w' h' <- G.get G.windowSize
			writeIORef fw $ fromIntegral w'
			writeIORef fh $ fromIntegral h'
			G.clearColor $= G.Color4 0 0 0 0
			G.clear [G.ColorBuffer]
			makeFieldColor . head =<< readIORef bgc
			sequence_ . reverse . catMaybes =<< readIORef actions
			join $ readIORef action
			swapBuffers
		actwc = do
			G.clearColor $= G.Color4 0 0 0 0
			G.clear [G.ColorBuffer]
			G.lineWidth $= 1.0
			ss1 <- readIORef str
			ss2 <- readIORef str2
			zipWithM_ (printString (-2.8)) [-1800, -1600 .. 1800] (reverse ss1 ++ ss2)
			swapBuffers
	currentWindow $= Just wt
	displayCallback $= atomicModifyIORef_ fc (+ 1) >> act
	currentWindow $= Just wc
	displayCallback $= act
	G.addTimerCallback 10 $ timerAction act
	G.reshapeCallback $= Just (\size -> G.viewport $= (G.Position 0 0, size))
	fcoord <- newIORef CoordCenter
	let f = Field{
		fChanged = fc,
		fAct = act,
		fCoordinates = fcoord,
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

		fBgcolor = bgc,

		fBusy = fb,
		fOnclick = click
	 }
	G.keyboardMouseCallback $= Just (processKeyboardMouse f)
	currentWindow $= Just wt
	G.keyboardMouseCallback $= Just (processKeyboardMouse f)
	return f

processKeyboardMouse :: Field -> G.Key -> G.KeyState -> G.Modifiers -> G.Position -> IO ()
processKeyboardMouse f (G.Char c) ks m p = do
	keyboardProc f c ks m p
	atomicModifyIORef_ (fChanged f) (+ 1)
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

closeField :: Field -> IO ()
closeField _ = G.leaveMainLoop

topleft, center :: Field -> IO ()
topleft = flip writeIORef CoordTopLeft . fCoordinates
center = flip writeIORef CoordCenter . fCoordinates

coordinates :: Field -> IO Coordinates
coordinates = readIORef . fCoordinates

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

fieldColor :: Field -> Color -> IO ()
fieldColor f clr = do
	atomicModifyIORef_ (fBgcolor f) (clr :)
	atomicModifyIORef_ (fActions f) (Nothing :)

makeFieldColor :: Color -> IO ()
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

drawLine :: Field -> Double -> Color -> Position -> Position -> IO ()
drawLine f w c p q = do
	atomicModifyIORef_ (fActions f) (Just (makeLineAction f p q c w) :)
	atomicModifyIORef_ (fChanged f) (+ 1)
	flush

makeLineAction :: Field -> Position -> Position -> Color -> Double -> IO ()
makeLineAction f p q c w = preservingMatrix $ do
	G.lineWidth $= fromRational (toRational w)
	G.color $ colorToColor4 c
	pp <- positionToVertex3 f p
	qq <- positionToVertex3 f q
	renderPrimitive Lines $ mapM_ vertex [pp, qq]

colorToColor4 :: Color -> G.Color4 GLfloat
colorToColor4 (RGB r g b) = G.Color4
	(fromIntegral r / 255) (fromIntegral g / 255) (fromIntegral b / 255) 0
colorToColor4 _ = error "colorToColor4: not implemented"

makeCharacterAction :: Field -> [Position] -> Color -> Color -> Double -> IO ()
makeCharacterAction f ps c lc lw = do
	ps' <- mapM (positionToPos f) ps
	vs <- mapM (positionToVertex3 f . posToPosition) $
		triangleToPositions $ toTriangles ps'
	vs' <- mapM (positionToVertex3 f) ps
	preservingMatrix $ do
		G.color $ colorToColor4 c
		renderPrimitive Triangles $ mapM_ vertex vs
		G.lineWidth $= fromRational (toRational lw)
		G.color $ colorToColor4 lc
		renderPrimitive LineLoop $ mapM_ vertex vs'

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

positionToVertex3 :: Field -> Position -> IO (Vertex2 GLfloat)
positionToVertex3 f (Center x y) = do
	w <- readIORef $ fWidth f
	h <- readIORef $ fHeight f
	return $ Vertex2
		(fromRational $ 2 * toRational x / fromIntegral w)
		(fromRational $ 2 * toRational y / fromIntegral h)
positionToVertex3 f (TopLeft x y) = do
	w <- readIORef $ fWidth f
	h <- readIORef $ fHeight f
	let	x' = 2 * toRational x / fromIntegral w - 1
		y' = 1 - 2 * toRational y / fromIntegral h
	return $ Vertex2 (fromRational x') (fromRational y')

writeString :: Field -> String -> Double -> Color -> Position ->
	String -> IO ()
writeString f _fname size clr (Center x_ y_) str =
	atomicModifyIORef_ (fActions f) (Just action :)
	where
	action = preservingMatrix $ do
		h <- readIORef $ fHeight f
		w <- readIORef $ fWidth f
		let	size' = size / 15
			ratio = 3.5 * fromIntegral h
			x_ratio = 2 * ratio / fromIntegral w
			y_ratio = 2 * ratio / fromIntegral h
			x = x_ratio * fromRational (toRational $ x_ / size')
			y = y_ratio * fromRational (toRational $ y_ / size')
			s = 1 / ratio * fromRational (toRational size')
		G.color $ colorToColor4 clr
		G.scale (s :: GLfloat) (s :: GLfloat) (s :: GLfloat)
		G.clearColor $= G.Color4 0 0 0 0
		G.translate (G.Vector3 x y 0 :: G.Vector3 GLfloat)
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

outputString :: Field -> String -> IO ()
outputString f = atomicModifyIORef_ (fString2 f) . (:)

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

keyboardProc :: Field -> Char -> G.KeyState -> G.Modifiers -> G.Position -> IO ()
keyboardProc f '\r' G.Down _ _ = do
	p <- readIORef $ fPrompt f
	str <- readIORef (fString f)
	atomicModifyIORef_ (fString2 f) (reverse str ++)
	writeIORef (fString f) [p]
	continue <- ($ drop (length p) $ concat str) =<< readIORef (fInputtext f)
	unless continue G.leaveMainLoop
keyboardProc f '\b' G.Down _ _ = do
	p <- readIORef $ fPrompt f
	atomicModifyIORef_ (fString f) $ \s -> case s of
		[""] -> [""]
		[ss] | length ss <= length p -> s
		_ -> case last s of
			"" -> init (init s) ++ [init $ last $ init s]
			_ -> init s ++ [init $ last s]
keyboardProc f c state _ _
	| state == G.Down = atomicModifyIORef_ (fString f) (`addToTail` c)
	| otherwise = return ()

addToTail :: [String] -> Char -> [String]
addToTail strs c
	| null strs = error "bad"
	| length (last strs) < 50 = init strs ++ [last strs ++ [c]]
	| otherwise = strs ++ [[c]]
