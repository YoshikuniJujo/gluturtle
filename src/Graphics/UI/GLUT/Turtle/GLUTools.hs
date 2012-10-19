module Graphics.UI.GLUT.Turtle.GLUTools (
	initialize,
	createWindow,
	printCommands,
	keyboardCallback,
	keyboardMouseCallback,
	displayAction,
	loop,
	windowColor,

	currentWindow,
--	separateLine,

	windowSize,
	setWindowSize,
	leaveUnless,

	module Graphics.UI.GLUT
) where

import Graphics.UI.GLUT hiding (
	initialize, createWindow, keyboardMouseCallback, currentWindow,
	windowSize)
import qualified Graphics.UI.GLUT as G
import System.Environment
import Control.Monad
import Data.IORef
import Data.IORef.Tools
import Control.Applicative

type Pos = Position

initialize :: IO [String]
initialize = do
	prgName <- getProgName
	rawArgs <- getArgs
	args <- G.initialize prgName rawArgs
	initialDisplayMode $= [RGBMode, DoubleBuffered]
	return args

createWindow :: String -> Int -> Int -> IO Window
createWindow name w h = do
	initialWindowSize $= Size (fromIntegral w) (fromIntegral h)
	G.createWindow name

printCommands :: G.Window -> [String] -> IO ()
printCommands win strs =
	concatMap reverse <$> mapM separateLine strs >>= printCommands_ win

printCommands_ :: G.Window -> [String] -> IO ()
printCommands_ win strs = do
	G.currentWindow $= Just win
	G.clearColor $= G.Color4 0 0 0 0
	G.clear [G.ColorBuffer]
	G.lineWidth $= 1.0
	zipWithM_ (printString (-2.8)) [-1800, -1600 .. 1800] strs
	G.swapBuffers

separateLine :: String -> IO [String]
separateLine "" = return []
separateLine str = do
	n <- getStringNum str 1
	rest <- separateLine (drop n str)
	return $ take n str : rest

getStringNum :: String -> Int -> IO Int
getStringNum str n
	| n >= length str = return n
	| otherwise = G.preservingMatrix $ do
		sw <- G.stringWidth G.Roman (take n str)
		if sw < 3900
			then getStringNum str (n + 1) else return n

printString :: G.GLfloat -> G.GLfloat -> String -> IO ()
printString x y str =
	G.preservingMatrix $ do
		G.scale (0.0005 :: G.GLfloat)  0.0005 0.0005
		G.clearColor $= G.Color4 0 0 0 0
		G.color (G.Color4 0 1 0 0 :: G.Color4 G.GLfloat)
		w <- G.stringWidth G.Roman "Stroke font"
		G.translate (G.Vector3 (x * fromIntegral w)
			y 0 :: G.Vector3 G.GLfloat)
		G.renderString G.Roman str

keyboardCallback ::
	(Char -> G.KeyState -> G.Modifiers -> IO ()) -> IO ()
keyboardCallback f = G.keyboardMouseCallback $= Just (\k ks m _ -> case k of
	G.Char chr -> f chr ks m
	_ -> return ())

keyboardMouseCallback ::
	(G.Key -> G.KeyState -> G.Modifiers -> (Double, Double) -> IO ()) -> IO ()
keyboardMouseCallback fun = (G.keyboardMouseCallback $=) $ Just $
	\k ks m (Position x y) ->fun k ks m (fromIntegral x, fromIntegral y)

displayAction :: IORef Int -> IO () -> IO ()
displayAction changed act = loop_ changed act >> G.displayCallback $= act

loop_ :: IORef Int -> IO a -> IO ()
loop_ changed act = G.addTimerCallback 10 $ timerAction changed act

loop :: IO a -> IO ()
loop act = G.addTimerCallback 10 $ timerAction' act

timerAction :: IORef Int -> IO a -> IO ()
timerAction changed act = do
	c <- readIORef changed
	when (c > 0) $ do
		_ <- act
		atomicModifyIORef_ changed (subtract 1)
	G.addTimerCallback 10 $ timerAction changed act

timerAction' :: IO a -> IO ()
timerAction' act = act >> G.addTimerCallback 10 (timerAction' act)

windowColor :: G.Color4 G.GLfloat -> IO ()
windowColor clr = G.preservingMatrix $ do
	G.color clr
	G.renderPrimitive G.Quads $ mapM_ G.vertex [
		G.Vertex2 (-1) (-1),
		G.Vertex2 (-1) 1,
		G.Vertex2 1 1,
		G.Vertex2 1 (-1) :: G.Vertex2 G.GLfloat ]

currentWindow :: Window -> IO ()
currentWindow = (G.currentWindow $=) . Just

windowSize :: IO (Int, Int)
windowSize = do
	G.Size w h <- G.get G.windowSize
	return (fromIntegral w, fromIntegral h)

setWindowSize :: Int -> Int -> IO ()
setWindowSize w h = (G.windowSize $=) $ Size (fromIntegral w) (fromIntegral h)

leaveUnless :: Bool -> IO ()
leaveUnless = flip unless G.leaveMainLoop
