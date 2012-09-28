import Graphics.UI.GLUT.Turtle
import Graphics.UI.GLUT
import Control.Concurrent

import System.Environment

main = do
	prgName <- getProgName
	rawArgs <- getArgs
	args <- initialize prgName rawArgs
	f <- openField
	t <- newTurtle f
--	speed t "slowest"
	threadDelay 1000000
	left t 45
	forward t 100
	mainLoop
