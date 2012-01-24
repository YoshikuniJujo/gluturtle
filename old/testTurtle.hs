import Graphics.X11.Turtle
import Control.Monad
import System.Random

spiral :: Double -> Double -> IO ()
spiral size angle
	| size > 100	= return ()
	| otherwise	= do
		forward size
		right angle
		spiral (size + 2) angle

qcircle :: IO ()
qcircle = replicateM_ 9 $ forward 10 >> right 10

leaf :: IO ()
leaf = qcircle >> right 90 >> qcircle

flower :: IO ()
flower = do
	left 90
	forward 50
	clear
	replicateM_ 9 $ leaf >> right 10
	right 180
	forward 200
	right 180
	forward 30
	right 20
	leaf

triangles :: Double -> IO ()
triangles size
	| size < 10	= return ()
	| otherwise	= do
		right 60
		replicateM_ 3 $ forward size >> right 120
		forward $ size / 2
		triangles $ size / 2

polygon :: Double -> Int -> IO ()
polygon size repeats =
	replicateM_ repeats $ forward size >> right (fromIntegral $ 360 `div` repeats)

randomTurtle :: IO ()
randomTurtle = sequence_ $ repeat $ do
	randomRIO (-180, 180) >>= left >> forward 15
	d <- distance 0 0
	when (d > 100) undo

squares :: Double -> IO ()
squares len
	| len <= 10	= return ()
	| otherwise	= do
		replicateM_ 4 $ forward len >> right 90
		right 90 >> forward len >> left 90
		squares (len / 2)

coch :: Double -> Double -> Int -> IO ()
coch len _ 0 = forward len
coch len d n = do
	coch (len / 3) d (n - 1)
	left d
	coch (len / 3) d (n - 1)
	right (d * 2)
	coch (len / 3) d (n - 1)
	left d
	coch (len / 3) d (n - 1)