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

import Control.Concurrent(ThreadId, forkIO, Chan, newChan, writeChan, readChan)
import Data.IORef(IORef, newIORef, readIORef, writeIORef)
import Data.IORef.Tools(atomicModifyIORef_)
import Data.Maybe
import System.Environment

--------------------------------------------------------------------------------

initialize :: IO [String]
initialize = do
	prgName <- getProgName
	rawArgs <- getArgs
	args <- G.initialize prgName rawArgs
	initialDisplayMode $= [RGBMode, DoubleBuffered]
	return args

prompt :: Field -> String -> IO ()
prompt f p = do
	writeIORef (fPrompt $ fConsole f) p
	atomicModifyIORef_ (fCommand $ fConsole f) (\ls -> init ls ++ [p ++ last ls])

data Coordinates = CoordTopLeft | CoordCenter

data Console = Console{
	fConsoleWindow :: Window,
	fPrompt :: IORef String,
	fCommand :: IORef [String],
	fHistory :: IORef [String],
	cChanged :: IORef Int,
	cChanFull :: IORef Bool,
	cChan :: Chan String
 }

data Field = Field{
	fConsole :: Console,

	fFieldWindow :: Window,
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

openConsole :: Int -> Int -> IO Console
openConsole w h = do
	fprompt <- newIORef ""
	fcommand <- newIORef [""]
	fhistory <- newIORef []
	cchanged <- newIORef 0
	cchanfull <- newIORef False
	cchan <- newChan

	initialWindowSize $= Size (fromIntegral w) (fromIntegral h)
	fconsole <- createWindow "console"

	let	actwc = do
			changed <- readIORef cchanged
			when (changed > 0) $ do
				G.currentWindow $= Just fconsole
				G.clearColor $= G.Color4 0 0 0 0
				G.clear [G.ColorBuffer]
				G.lineWidth $= 1.0
				ss1 <- readIORef fcommand
				ss2 <- readIORef fhistory
				zipWithM_ (printString (-2.8)) [-1800, -1600 .. 1800] (reverse ss1 ++ ss2)
				swapBuffers
				atomicModifyIORef_ cchanged (subtract 1)

		c = Console{
			fConsoleWindow = fconsole,
			fPrompt = fprompt,
			fCommand = fcommand,
			fHistory = fhistory,
			cChanged = cchanged,
			cChanFull = cchanfull,
			cChan = cchan
		 }
	G.keyboardMouseCallback $= Just (\k ks m p -> case k of
		G.Char chr -> do
			atomicModifyIORef_ cchanged (+ 1)
			processKeyboard c chr ks m p
		_ -> return ())
	G.addTimerCallback 10 $ atomicModifyIORef_ cchanged (+ 1) >> timerAction actwc
	displayCallback $= atomicModifyIORef_ cchanged (+ 1) >> actwc
	return c

openField :: String -> Int -> Int -> IO Field
openField name w h = do
	fconsole <- openConsole w h

	fsize <- newIORef (w, h)
	fcoord <- newIORef CoordCenter
	fbgcolor <- newIORef [RGB 255 255 255]

	faction <- newIORef $ return ()
	factions <- newIORef []

	fchanged <- newIORef 0
	finputtext <- newIORef $ const $ return True
	fclick <- newIORef (\_ _ _ -> return True)

	initialWindowSize $= Size (fromIntegral w) (fromIntegral h)
	ffield <- createWindow name

	let	act = do
			change <- readIORef fchanged
			when (change > 0) $ do
				currentWindow $= Just ffield
				actwt
				atomicModifyIORef_ fchanged (subtract 1)
		actChan = do
			full <- readIORef $ cChanFull fconsole
			when full $ do
				cmd <- readChan $ cChan fconsole
				continue <- readIORef finputtext >>= ($ cmd)
				writeIORef (cChanFull fconsole) False
				unless continue G.leaveMainLoop
		actwt = do
			Size w' h' <- G.get G.windowSize
			writeIORef fsize $ (fromIntegral w', fromIntegral h')
			G.clearColor $= G.Color4 0 0 0 0
			G.clear [G.ColorBuffer]
			makeFieldColor . head =<< readIORef fbgcolor
			sequence_ . reverse . catMaybes =<< readIORef factions
			join $ readIORef faction
			swapBuffers
	displayCallback $= atomicModifyIORef_ fchanged (+ 1) >> act
	G.addTimerCallback 10 $ timerAction act
	G.addTimerCallback 10 $ timerAction actChan
	G.reshapeCallback $= Just (\size -> G.viewport $= (G.Position 0 0, size))
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
	writeIORef (fSize f) (w, h)
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
	(w, h) <- readIORef $ fSize f
	return $ Vertex2
		(fromRational $ 2 * toRational x / fromIntegral w)
		(fromRational $ 2 * toRational y / fromIntegral h)
positionToVertex3 f (TopLeft x y) = do
	(w, h) <- readIORef $ fSize f
	let	x' = 2 * toRational x / fromIntegral w - 1
		y' = 1 - 2 * toRational y / fromIntegral h
	return $ Vertex2 (fromRational x') (fromRational y')

writeString :: Field -> String -> Double -> Color -> Position ->
	String -> IO ()
writeString f _fname size clr (Center x_ y_) str =
	atomicModifyIORef_ (fActions f) (Just action :)
	where
	action = preservingMatrix $ do
		(w, h) <- readIORef $ fSize f
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
outputString f = atomicModifyIORef_ (fHistory $ fConsole f) . (:)

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

processKeyboard :: Console -> Char -> G.KeyState -> G.Modifiers -> G.Position -> IO ()
processKeyboard c '\r' G.Down _ _ = do
	p <- readIORef $ fPrompt c
	str <- readIORef (fCommand c)
	atomicModifyIORef_ (fHistory c) (reverse str ++)
	writeIORef (fCommand c) [p]
	writeIORef (cChanFull c) True
	writeChan (cChan c) $ drop (length p) $ concat str
processKeyboard c '\b' G.Down _ _ = do
	p <- readIORef $ fPrompt c
	atomicModifyIORef_ (fCommand c) $ \s -> case s of
		[""] -> [""]
		[ss] | length ss <= length p -> s
		_ -> case last s of
			"" -> init (init s) ++ [init $ last $ init s]
			_ -> init s ++ [init $ last s]
processKeyboard c chr state _ _
	| state == G.Down = atomicModifyIORef_ (fCommand c) (`addToTail` chr)
	| otherwise = return ()

keyboardProc :: Field -> Char -> G.KeyState -> G.Modifiers -> G.Position -> IO ()
keyboardProc f '\r' G.Down _ _ = do
	p <- readIORef $ fPrompt $ fConsole f
	str <- readIORef (fCommand $ fConsole f)
	atomicModifyIORef_ (fHistory $ fConsole f) (reverse str ++)
	writeIORef (fCommand $ fConsole f) [p]
	continue <- ($ drop (length p) $ concat str) =<< readIORef (fInputtext f)
	unless continue G.leaveMainLoop
keyboardProc f '\b' G.Down _ _ = do
	p <- readIORef $ fPrompt $ fConsole f
	atomicModifyIORef_ (fCommand $ fConsole f) $ \s -> case s of
		[""] -> [""]
		[ss] | length ss <= length p -> s
		_ -> case last s of
			"" -> init (init s) ++ [init $ last $ init s]
			_ -> init s ++ [init $ last s]
keyboardProc f c state _ _
	| state == G.Down = atomicModifyIORef_ (fCommand $ fConsole f) (`addToTail` c)
	| otherwise = return ()

addToTail :: [String] -> Char -> [String]
addToTail strs c
	| null strs = error "bad"
	| length (last strs) < 50 = init strs ++ [last strs ++ [c]]
	| otherwise = strs ++ [[c]]
