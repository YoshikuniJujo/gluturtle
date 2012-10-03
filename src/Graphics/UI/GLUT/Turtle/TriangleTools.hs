module Graphics.UI.GLUT.Turtle.TriangleTools (
	Pos,
	far,
	index3,
	deleteIndex,

	distant3,
	within,
	isRight,
	online,
	deleteOnline,
	draw,

	pa, pb, pc, pd, pe
) where

import Graphics.UI.GLUT
import System.Environment

far :: [Pos] -> Int
far = maximumIndex . map distance2

deleteOnline :: [Pos] -> [Pos]
deleteOnline xs = init $ tail $ dol $ last xs : xs ++ [head xs]

dol :: [Pos] -> [Pos]
dol [a, b] = [a, b]
dol (a : ps@(b : ps'@(c : _)))
	| online (a, b, c) = a : dol ps'
	| otherwise = a : dol ps
dol p = error $ "dol: not implemented " ++ show p

maximumIndex :: Ord a => [a] -> Int
maximumIndex = fst . maximumIndexGen

index3 :: [a] -> Int -> (a, a, a)
index3 xs i = (xs' !! i, xs' !! (i + 1), xs' !! (i + 2))
	where
	xs' = last xs : xs ++ [head xs]

deleteIndex :: [a] -> Int -> [a]
deleteIndex xs i = take i xs ++ drop (i + 1) xs

maximumIndexGen :: Ord a => [a] -> (Int, a)
maximumIndexGen [x] = (0, x)
maximumIndexGen (x : xs)
	| x >= x' = (0, x)
	| otherwise = (i + 1, x')
	where
	(i, x') = maximumIndexGen xs
maximumIndexGen _ = error "maximumIndexGen: not implemented"

draw :: [Pos] -> [(Pos, Pos, Pos)] -> IO ()
draw pl trs = do
	prgName <- getProgName
	rawArgs <- getArgs
	_args <- initialize prgName rawArgs
	_ <- createWindow "GLTest"
	displayCallback $= do
		color (Color4 1 1 1 0 :: Color4 GLfloat)
		drawTriangles trs -- [((50, 50), (-50, -50), (0, 50))]
		color (Color4 1 0 0 0 :: Color4 GLfloat)
		drawPolyline pl -- [(50, 50), (-50, -50), (0, 50), (-50, 80)]
		swapBuffers
		flush
	mainLoop

drawTriangles :: [(Pos, Pos, Pos)] -> IO ()
drawTriangles ps = preservingMatrix $
	renderPrimitive Triangles $ mapM_ vertex $ trianglesToVertex2 ps

trianglesToVertex2 :: [(Pos, Pos, Pos)] -> [Vertex2 GLfloat]
trianglesToVertex2 [] = []
trianglesToVertex2 ((a, b, c) : rest) =
	toVertex2 a : toVertex2 b : toVertex2 c : trianglesToVertex2 rest

toVertex2 :: (Double, Double) -> Vertex2 GLfloat
toVertex2 (x, y) = Vertex2
	(fromRational $ toRational x / 20) (fromRational $ toRational y / 20)

drawPolyline :: [Pos] -> IO ()
drawPolyline ps = preservingMatrix $
	renderPrimitive LineLoop $ mapM_ (vertex . toVertex2) ps

type Pos = (Double, Double)

isRight :: (Pos, Pos, Pos) -> Bool
isRight ((xa, ya), (xb, yb), (xc, yc))
	| 0 < xd * ye - xe * yd = True
	| 0 == xd * ye - xe * yd = error "bad triangle"
	| otherwise = False
	where
	(xd, yd) = (xa - xb, ya - yb)
	(xe, ye) = (xc - xb, yc - yb)

distance2 :: Floating a => (a, a) -> a
distance2 (x, y) = x ** 2 + y ** 2

maximumby32 :: Ord b => (a -> b) -> [a] -> ((a, a, a), [a])
maximumby32 b xs = maximumby3Gen2 b $ last xs : xs ++ [head xs]

maximumby3Gen2 :: Ord b => (a -> b) -> [a] -> ((a, a, a), [a])
maximumby3Gen2 _ [x, y, z] = ((x, y, z), [x, z])
maximumby3Gen2 b (x : ys@(y : (zs@(z : _))))
	| b y > b y' = ((x, y, z), x : zs)
	| otherwise = (t, x : ps)
	where
	(t@(_, y', _), ps) = maximumby3Gen2 b ys
maximumby3Gen2 _ _ = error "maximumby3Gen2: not implemented"

distant3 :: [Pos] -> ((Pos, Pos, Pos), [Pos])
distant3 ps = let (t, ps') = maximumby32 distance2 ps in (t, init $ tail ps')

within :: (Pos, Pos, Pos) -> Pos -> Bool
within (a, b, c) d
	| online (d, a, b) || online (d, b, c) || online (d, c, a) = False
	| otherwise = isRight (a, b, c) &&
		isRight (d, a, b) && isRight (d, b, c) && isRight (d, c, a)

online :: (Pos, Pos, Pos) -> Bool
online ((xa, ya), (xb, yb), (xc, yc)) = 0 == xd * ye - xe * yd
	where
	(xd, yd) = (xa - xb, ya - yb)
	(xe, ye) = (xc - xb, yc - yb)

pa, pb, pc, pd, pe :: Pos
pa = (0, 4)
pb = (4, 4)
pc = (4, 0)
pd = (3, 3)
pe = (2, 2)

{-
deleteOnline :: [Pos] -> [Pos]
deleteOnline xs = deleteOnline' $ last xs : xs ++ [head xs]

deleteOnline' :: [Pos] -> [Pos]
deleteOnline' ps@[_, _] = ps
deleteOnline' (a : ps@(b : c : ps'))
	| online (a, b, c) = deleteOnline' $ a' : b' : ps'
	| otherwise = a : deleteOnline' ps
	where
	(a', b') = deleteOnlineGen (a, b, c)
-}
