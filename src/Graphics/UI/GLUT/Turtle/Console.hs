module Graphics.UI.GLUT.Turtle.Console (
	Console,
	openConsole,
	consolePrompt,
	consoleOutput,
	consoleKeyboard,
	consoleCommand
) where

import Graphics.UI.GLUT.Turtle.GLUTools(
	KeyState(..), Modifiers,
	createWindow, printCommands, keyboardCallback, displayAction)
import Data.IORef(IORef, newIORef, readIORef, writeIORef)
import Data.IORef.Tools(atomicModifyIORef_)
import Control.Applicative((<$>))
import Control.Concurrent.STM(atomically)
import Control.Concurrent.STM.TChan(
	TChan, newTChan, readTChan, writeTChan, isEmptyTChan)

--------------------------------------------------------------------------------

data Console = Console{
	cPrompt :: IORef String,
	cCommand :: IORef [String],
	cHistory :: IORef [String],
	cChanged :: IORef Int,
	cResult :: TChan String
 }

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
			cResult = cchan }
	keyboardCallback $ consoleKeyboard c
	displayAction cchanged $ do
		prmpt <- readIORef cprompt
		cmd <- readIORef ccommand
		hst <- readIORef chistory
		printCommands cwindow 1.0 $ mergeToHistory prmpt cmd hst
	return c

consolePrompt :: Console -> String -> IO ()
consolePrompt c p = writeIORef (cPrompt c) p

consoleOutput :: Console -> String -> IO ()
consoleOutput c str = atomicModifyIORef_ (cHistory c) (str :)

consoleKeyboard :: Console -> Char -> KeyState -> Modifiers -> IO ()
consoleKeyboard console '\r' Down _ = do
	atomicModifyIORef_ (cChanged console) succ
	prmpt <- readIORef $ cPrompt console
	cmd <- readIORef $ cCommand console
	atomicModifyIORef_ (cHistory console) $ mergeToHistory prmpt cmd
	writeIORef (cCommand console) [""]
	atomically $ writeTChan (cResult console) $ concat cmd
consoleKeyboard console '\b' Down _ = do
	atomicModifyIORef_ (cChanged console) succ
	atomicModifyIORef_ (cCommand console) $ \s -> case (init s, last s) of
		([], "") -> [""]
		(i, "") -> init i ++ [init $ last i]
		(i, l) -> i ++ [init l]
consoleKeyboard c chr Down _ = do
	pre <- readIORef $ cPrompt c
	atomicModifyIORef_ (cChanged c) succ
	atomicModifyIORef_ (cCommand c) $ \str -> addToTail pre str chr
consoleKeyboard _ _ _ _ = return ()

consoleCommand :: Console -> IO (Maybe String)
consoleCommand console = atomically $ do
	emp <- isEmptyTChan $ cResult console
	if emp then return Nothing else
		Just <$> readTChan (cResult console)

addToTail :: String -> [String] -> Char -> [String]
addToTail _ [] _ = error "bad"
addToTail pre [str] c
	| length (pre ++ str) < 50 = [str ++ [c]]
	| otherwise = [str] ++ [[c]]
addToTail _ strs c
	| length (last strs) < 50 = init strs ++ [last strs ++ [c]]
	| otherwise = strs ++ [[c]]

mergeToHistory :: String -> [String] -> [String] -> [String]
mergeToHistory _ [] = error "mergeToHistory: bad"
mergeToHistory prmpt (cstr : cstrs) = (reverse ((prmpt ++ cstr) : cstrs) ++)
