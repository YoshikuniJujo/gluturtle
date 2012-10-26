import Graphics.UI.GLUT.Turtle
import Graphics.UI.GLUT hiding (position, initialize, clear)

main :: IO ()
main = do
	_args <- initialize
	f <- openField "test" 640 480
	c <- openConsole "console" 640 480
	consolePrompt c "> "
	setConsole f c
	t <- newTurtle f
	onclick f $ \_bn x y -> goto t x y >> return True
	oncommand f (processInput f c t)
--	speed t "slowest"
--	fillcolor t ((255, 255, 255) :: (Int, Int, Int))
--	pencolor t ((255, 255, 255) :: (Int, Int, Int))
--	threadDelay 1000000
--	left t 45
--	forward t 100
	mainLoop

processInput :: Field -> Console -> Turtle -> String -> IO Bool
processInput _ _ t "forward" = forward t 100 >> return True
processInput _ _ t "left" = left t 90 >> return True
processInput _ _ t "right" = right t 90 >> return True
processInput _ _ t "begin" = beginfill t >> return True
processInput _ _ t "end" = endfill t >> return True
processInput _ _ t "turtle" = shape t "turtle" >> return True
processInput _ _ t "stamp" = stamp t >> return True
processInput _ _ t "bold" = pensize t 7 >> return True
processInput _ _ t "verybold" = pensize t 10 >> return True
processInput _ _ t "big" = shapesize t 3 3 >> return True
processInput _ _ t "blue" = pencolor t ((0, 0, 255) :: (Int, Int, Int)) >> return True
processInput _ _ t "fblue" = fillcolor t ((0, 0, 255) :: (Int, Int, Int)) >> return True
processInput _ _ t "yellow" = pencolor t ((255, 255, 0) :: (Int, Int, Int)) >> return True
processInput _ _ t "normal" = pensize t 1 >> return True
processInput _ _ _ "exit" = return False
processInput _ c t "position" = position t >>= consoleOutput c . show >> return True
processInput _ _ t "penup" = penup t >> return True
processInput _ _ t "pendown" = pendown t >> return True
processInput _ _ t "message" = write t "KochiGothic" 100 "Hello, world!" >> return True
processInput _ _ _ "0very1very2very3very4very5very6very7very8very9very0very-long-line"
	= putStrLn "very long line" >> return True
processInput _ _ t "hide" = hideturtle t >> return True
processInput _ _ t "bred" = bgcolor t ((255, 0, 0) :: (Int, Int, Int)) >> return True
processInput _ _ t "bgreen" = bgcolor t ((0, 255, 0) :: (Int, Int, Int)) >> return True
processInput _ _ t "home" = goto t 0 0 >> return True
processInput _ _ t "undo" = undo t >> return True
processInput f _ _ "size" = setFieldSize f 100 200 >> return True
processInput f _ _ "topleft" = topleft f >> return True
processInput f _ _ "center" = center f >> return True
processInput _ _ t "goto100" = goto t 100 100 >> return True
processInput _ _ t "clear" = clear t >> return True
processInput _ _ _ _ = return True
