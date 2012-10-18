module Graphics.UI.GLUT.Turtle.Console (
	Console(..),
	openConsole,
	processKeyboard
) where

import Data.IORef
import Data.IORef.Tools
import Control.Concurrent
import Graphics.UI.GLUT.Turtle.GLUTools as G

data Console = Console{
	cPrompt :: IORef String,
	cCommand :: IORef [String],
	cHistory :: IORef [String],
	cChanged :: IORef Int,
	cChanChanged :: IORef Int,
	cChan :: Chan String
 }

openConsole :: String -> Int -> Int -> IO Console
openConsole name w h = do
	cwindow <- createWindow name w h
	cprompt <- newIORef ""
	ccommand <- newIORef [""]
	chistory <- newIORef []
	cchanged <- newIORef 1
	cchanchanged <- newIORef 0
	cchan <- newChan
	let	c = Console{
			cPrompt = cprompt,
			cCommand = ccommand,
			cHistory = chistory,
			cChanged = cchanged,
			cChanChanged = cchanchanged,
			cChan = cchan }
	keyboardCallback $ processKeyboard c
	displayAction cchanged $ do
		cmd <- readIORef ccommand
		hst <- readIORef chistory
		printLines cwindow 1.0 $ reverse cmd ++ hst
	return c

processKeyboard ::
	Console -> Char -> G.KeyState -> G.Modifiers -> G.Position -> IO ()
processKeyboard console '\r' G.Down _ _ = do
	atomicModifyIORef_ (cChanged console) (+ 1)
	p <- readIORef $ cPrompt console
	str <- readIORef (cCommand console)
	atomicModifyIORef_ (cHistory console) (reverse str ++)
	writeIORef (cCommand console) [p]
	atomicModifyIORef_ (cChanChanged console) (+ 1)
	writeChan (cChan console) $ drop (length p) $ concat str
processKeyboard c '\b' G.Down _ _ = do
	atomicModifyIORef_ (cChanged c) (+ 1)
	p <- readIORef $ cPrompt c
	atomicModifyIORef_ (cCommand c) $ \s -> case s of
		[""] -> [""]
		[ss] | length ss <= length p -> s
		_ -> case (init s, last s) of
			(i, "") -> init i ++ [init $ last i]
			(i, l) -> i ++ [init l]
processKeyboard c chr G.Down _ _ = do
	atomicModifyIORef_ (cChanged c) (+ 1)
	atomicModifyIORef_ (cCommand c) (`addToTail` chr)
processKeyboard _ _ _ _ _ = return ()

addToTail :: [String] -> Char -> [String]
addToTail [] _ = error "bad"
addToTail strs c
	| length (last strs) < 50 = init strs ++ [last strs ++ [c]]
	| otherwise = strs ++ [[c]]
