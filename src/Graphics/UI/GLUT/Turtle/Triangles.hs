module Graphics.UI.GLUT.Turtle.Triangles(toTriangles) where

import Graphics.UI.GLUT.Turtle.TriangleTools

isHoleRight :: [Pos] -> Bool
isHoleRight ps
	| length (deleteOnline $ deletePoint ps) < 3 = True
isHoleRight ps = isRight tri
	where
	i = far (deleteOnline $ deletePoint ps)
	tri = index3 (deleteOnline $ deletePoint ps) i

toTriangles :: [Pos] -> [(Pos, Pos, Pos)]
toTriangles ps
	| length ps < 3 = []
	| isHoleRight ps = toTriangles' ps
	| otherwise = toTriangles' $ reverse ps

toTriangles' :: [Pos] -> [(Pos, Pos, Pos)]
toTriangles' ps
	| length (deleteOnline $ deletePoint ps) < 3 = []
	| otherwise = toTrianglesTop $ deleteOnline $ deletePoint ps

deletePoint :: Eq a => [a] -> [a]
deletePoint [] = []
deletePoint [p] = [p]
deletePoint pall@(p0 : ps)
	| p0 == last ps = ps
	| otherwise = pall

toTrianglesTop :: [Pos] -> [(Pos, Pos, Pos)]
toTrianglesTop [a, b, c] = [(a, b, c)]
toTrianglesTop ps
	| any (within tri) ps = toTrianglesNext (isRight tri) i ps
	| otherwise = tri : toTrianglesTop rest
	where
	i = far ps
	tri = index3 ps i
	rest = deleteIndex ps i

toTrianglesNext :: Bool -> Int -> [Pos] -> [(Pos, Pos, Pos)]
toTrianglesNext r i ps
	| isRight tri /= r || any (within tri) ps = toTrianglesNext r next ps
	| otherwise = tri : toTrianglesTop rest
	where
	tri = index3 ps i
	rest = deleteIndex ps i
	next = if i == 0 then length ps - 1 else i - 1
