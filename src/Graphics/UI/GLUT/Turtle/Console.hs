module Graphics.UI.GLUT.Turtle.Console (
	Console(..),
	openConsole,
	processKeyboard,
	prompt,
	outputString
) where

import Graphics.UI.GLUT.Turtle.GLUTools(
	KeyState(..), Modifiers, Position,
	createWindow, printLines, keyboardCallback, displayAction)
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Data.IORef(IORef, newIORef, readIORef, writeIORef)
import Data.IORef.Tools(atomicModifyIORef_)

--------------------------------------------------------------------------------

data Console = Console{
	cPrompt :: IORef String,
	cCommand :: IORef [String],
	cHistory :: IORef [String],
	cChanged :: IORef Int,
	cChan :: TChan String
 }

prompt :: Console -> String -> IO ()
prompt c p = do
	writeIORef (cPrompt c) p
	atomicModifyIORef_ (cCommand c) (\ls -> init ls ++ [p ++ last ls])

outputString :: Console -> String -> IO ()
outputString c str = atomicModifyIORef_ (cHistory c) (str :)

openConsole :: String -> Int -> Int -> IO Console
openConsole name w h = do
	cwindow <- createWindow name w h
	cprompt <- newIORef ""
	ccommand <- newIORef [""]
	chistory <- newIORef []
	cchanged <- newIORef 1
	cchan <- atomically newTChan
	let	c = Console{
			cPrompt = cprompt,
			cCommand = ccommand,
			cHistory = chistory,
			cChanged = cchanged,
			cChan = cchan }
	keyboardCallback $ processKeyboard c
	displayAction cchanged $ do
		cmd <- readIORef ccommand
		hst <- readIORef chistory
		printLines cwindow 1.0 $ reverse cmd ++ hst
	return c

processKeyboard ::
	Console -> Char -> KeyState -> Modifiers -> Position -> IO ()
processKeyboard console '\r' Down _ _ = do
	atomicModifyIORef_ (cChanged console) (+ 1)
	p <- readIORef $ cPrompt console
	str <- readIORef (cCommand console)
	atomicModifyIORef_ (cHistory console) (reverse str ++)
	writeIORef (cCommand console) [p]
	atomically $ writeTChan (cChan console) $ drop (length p) $ concat str
processKeyboard c '\b' Down _ _ = do
	atomicModifyIORef_ (cChanged c) (+ 1)
	p <- readIORef $ cPrompt c
	atomicModifyIORef_ (cCommand c) $ \s -> case s of
		[""] -> [""]
		[ss] | length ss <= length p -> s
		_ -> case (init s, last s) of
			(i, "") -> init i ++ [init $ last i]
			(i, l) -> i ++ [init l]
processKeyboard c chr Down _ _ = do
	atomicModifyIORef_ (cChanged c) (+ 1)
	atomicModifyIORef_ (cCommand c) (`addToTail` chr)
processKeyboard _ _ _ _ _ = return ()

addToTail :: [String] -> Char -> [String]
addToTail [] _ = error "bad"
addToTail strs c
	| length (last strs) < 50 = init strs ++ [last strs ++ [c]]
	| otherwise = strs ++ [[c]]
