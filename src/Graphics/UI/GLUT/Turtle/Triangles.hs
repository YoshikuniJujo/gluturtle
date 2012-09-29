module Graphics.UI.GLUT.Turtle.Triangles(toTriangles) where

import Graphics.UI.GLUT.Turtle.TriangleTools

ps = [pa, pb, pc, pd]
ps' = [(1, 3), (2.1, 2), (3, 3.1), (5, 2), (4, 1), (2, 1.5), (1, 1.1)]

main = draw ps' $ toTriangles ps'

toTriangles :: [Pos] -> [(Pos, Pos, Pos)]
toTriangles ps = toTrianglesTop ps'
--	| isRight tri = toTrianglesTop ps'
--	| otherwise = toTrianglesTop $ reverse ps'
	where
	ps' = deleteOnline ps
	(tri, rest) = distant3 ps

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
