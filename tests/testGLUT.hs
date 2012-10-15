import Graphics.UI.GLUT.Turtle
import Graphics.UI.GLUT hiding (position, initialize)

main :: IO ()
main = do
	_args <- initialize
	f <- openField "test" 640 480
	prompt f "> "
	t <- newTurtle f
	oninputtext f (processInput f t)
--	speed t "slowest"
--	fillcolor t ((255, 255, 255) :: (Int, Int, Int))
--	pencolor t ((255, 255, 255) :: (Int, Int, Int))
--	threadDelay 1000000
--	left t 45
--	forward t 100
	mainLoop

processInput :: Field -> Turtle -> String -> IO Bool
processInput _ t "forward" = forward t 100 >> return True
processInput _ t "left" = left t 90 >> return True
processInput _ t "right" = right t 90 >> return True
processInput _ t "begin" = beginfill t >> return True
processInput _ t "end" = endfill t >> return True
processInput _ t "turtle" = shape t "turtle" >> return True
processInput _ t "stamp" = stamp t >> return True
processInput _ t "bold" = pensize t 7 >> return True
processInput _ t "verybold" = pensize t 10 >> return True
processInput _ t "big" = shapesize t 3 3 >> return True
processInput _ t "blue" = pencolor t ((0, 0, 255) :: (Int, Int, Int)) >> return True
processInput _ t "fblue" = fillcolor t ((0, 0, 255) :: (Int, Int, Int)) >> return True
processInput _ t "yellow" = pencolor t ((255, 255, 0) :: (Int, Int, Int)) >> return True
processInput _ t "normal" = pensize t 1 >> return True
processInput _ _ "exit" = return False
processInput f t "position" = position t >>= outputString f . show >> return True
processInput _ t "penup" = penup t >> return True
processInput _ t "pendown" = pendown t >> return True
processInput _ t "message" = write t "KochiGothic" 100 "Hello, world!" >> return True
processInput _ _ "0very1very2very3very4very5very6very7very8very9very0very-long-line"
	= putStrLn "very long line" >> return True
processInput _ t "hide" = hideturtle t >> return True
processInput _ t "bred" = bgcolor t ((255, 0, 0) :: (Int, Int, Int)) >> return True
processInput _ t "bgreen" = bgcolor t ((0, 255, 0) :: (Int, Int, Int)) >> return True
processInput _ t "home" = goto t 0 0 >> return True
processInput _ t "undo" = undo t >> return True
processInput f _ "size" = setFieldSize f 100 200 >> return True
processInput _ _ _ = return True
