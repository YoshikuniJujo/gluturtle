import Graphics.UI.GLUT.Turtle
import Graphics.UI.GLUT hiding (position)
import Control.Concurrent

import System.Environment

main = do
	prgName <- getProgName
	rawArgs <- getArgs
	args <- initialize prgName rawArgs
	f <- openField
	t <- newTurtle f
	oninputtext f (processInput f t)
--	speed t "slowest"
	fillcolor t (255, 255, 255)
	pencolor t (255, 255, 255)
--	threadDelay 1000000
--	left t 45
--	forward t 100
	mainLoop

processInput _ t "forward" = forward t 100 >> return True
processInput _ t "left" = left t 90 >> return True
processInput _ t "begin" = beginfill t >> return True
processInput _ t "end" = endfill t >> return True
processInput _ t "turtle" = shape t "turtle" >> return True
processInput _ t "stamp" = stamp t >> return True
processInput _ t "bold" = pensize t 3 >> return True
processInput _ t "big" = shapesize t 3 3 >> return True
processInput _ t "blue" = pencolor t (0, 0, 255) >> return True
processInput _ t "fblue" = fillcolor t (0, 0, 255) >> return True
processInput _ t "yellow" = pencolor t (255, 255, 0) >> return True
processInput _ t "normal" = pensize t 1 >> return True
processInput _ t "exit" = return False
processInput f t "position" = position t >>= outputString f . show >> return True
processInput _ _ _ = return True
