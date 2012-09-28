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
	oninputtext f (processInput t)
--	speed t "slowest"
	pencolor t (255, 255, 255)
--	threadDelay 1000000
--	left t 45
--	forward t 100
	mainLoop

processInput t "forward" = forward t 100 >> return True
processInput t "left" = left t 90 >> return True
processInput t "begin" = beginfill t >> return True
processInput t "end" = endfill t >> return True
processInput t "turtle" = shape t "turtle" >> return True
processInput t "stamp" = stamp t >> return True
processInput t "bold" = pensize t 3 >> return True
processInput t "big" = shapesize t 3 3 >> return True
processInput t "blue" = pencolor t (0, 0, 255) >> return True
processInput t _ = return True
