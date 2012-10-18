module Graphics.UI.GLUT.Turtle.Move(
	initialize,

	-- * types
	Field,
	Console,
	Coordinates(..),

	-- * process Field
	openField,
	openConsole,
	setConsole,
	closeField,
	topleft,
	center,
	coordinates,
	fieldSize,
	setFieldSize,

	-- * draws
	forkField,
	flushField,
	clearCharacter,
	moveTurtle,

	-- * event
	oninputtext,
	onclick,
	onrelease,
	ondrag,
	onmotion,
	onkeypress,
	ontimer,
--	addLayer,
--	addCharacter,

	consoleOutput,
	consolePrompt
) where

import Graphics.UI.GLUT.Turtle.State(TurtleState(..), makeShape)
import Graphics.UI.GLUT.Turtle.Field(consolePrompt, initialize, setFieldSize,
	Field, Coordinates(..), openConsole, setConsole, Console,
	openField, closeField, coordinates, topleft, center,
	fieldSize, forkField, flushField,
	clearCharacter,
	oninputtext, onclick, onrelease, ondrag, onmotion, onkeypress, ontimer,
	fieldColor, drawLine, fillRectangle, fillPolygon, writeString,
	drawImage, undoField, drawCharacter, drawCharacterAndLine,
	consoleOutput)
import Text.XML.YJSVG(SVG(..), Position(..))
import qualified Text.XML.YJSVG as S(topleft)

import Control.Concurrent(threadDelay)
import Control.Monad(when, unless, forM_)
import Data.Maybe(isJust)

--------------------------------------------------------------------------------

moveTurtle :: Field -> TurtleState -> TurtleState -> IO ()
moveTurtle _ _ TurtleState{sleep = Just t} = threadDelay $ 1000 * t
moveTurtle f _ TurtleState{flush = True} = flushField f True $ return ()
moveTurtle f t0 t1 = do
	(w, h) <- fieldSize f
	when (undo t1) $ fl $ do
		when (clear t0) redraw
		when (isJust $ draw t0) $ do
			undoField f
			when (visible t1) $ drawTtl (direction t0) $ position t0
	when (visible t1) $ do
		forM_ (directions t0 t1) $ \dir -> fl $
			drawTtl dir (position t0) >> threadDelay (interval t0)
		forM_ (positions w h t0 t1) $ \p -> fl $
			drawTtl (direction t1) p >> threadDelay (interval t0)
		fl $ drawTtl (direction t1) $ position t1
	when (visible t0 && not (visible t1)) $ fl $ clearCharacter f
--	when (clear t1) $ fl $ clearLayer l
	unless (undo t1) $ fl $ maybe (return ()) (drawSVG f) (draw t1)
	where
	fl = flushField f $ stepbystep t0
	redraw = mapM_ (drawSVG f) $ reverse $ drawed t1
	drawTtl dir pos = drawTurtle f t1 dir pos begin
	begin	| undo t1 && pendown t0 = Just $ position t1
		| pendown t1 = Just $ position t0
		| otherwise = Nothing

drawSVG :: Field -> SVG -> IO ()
drawSVG f (Line p0 p1 clr lw) = drawLine f lw clr p0 p1
drawSVG f (Rect pos w h 0 fc _) = fillRectangle f pos w h fc
drawSVG f (Polyline ps fc lc lw) = fillPolygon f ps fc lc lw
drawSVG f (Fill clr) = fieldColor f clr
drawSVG f (Text pos sz clr fnt str) = writeString f fnt sz clr pos str
drawSVG f (Image pos w h fp) = drawImage f fp pos w h
drawSVG _ _ = error "not implemented"

positions :: Double -> Double -> TurtleState -> TurtleState -> [Position]
positions w h t0 t1 =
	maybe [] (mkPositions w h (position t0) (position t1)) $ positionStep t0

mkPositions :: Double -> Double -> Position -> Position -> Double -> [Position]
mkPositions w h p1 p2 step = case (p1, p2) of
	(Center x0 y0, Center x1 y1) -> map (uncurry Center) $ mp x0 y0 x1 y1
	(TopLeft x0 y0, TopLeft x1 y1) -> map (uncurry TopLeft) $ mp x0 y0 x1 y1
	_ -> mkPositions w h (S.topleft w h p1) (S.topleft w h p2) step
	where
	mp x0 y0 x1 y1 = let dist = ((x1 - x0) ** 2 + (y1 - y0) ** 2) ** (1 / 2)
		in take (floor $ dist / step) $ zip
			[x0, x0 + step * (x1 - x0) / dist .. ]
			[y0, y0 + step * (y1 - y0) / dist .. ]

directions :: TurtleState -> TurtleState -> [Double]
directions t0 t1 = case directionStep t0 of
	Nothing -> []
	Just step -> [ds, ds + dd .. de - dd]
		where
		dd = if de > ds then step else - step
		ds = direction t0
		de = direction t1

drawTurtle :: Field -> TurtleState -> Double -> Position ->
	Maybe Position -> IO ()
drawTurtle f ts@TurtleState{fillcolor = fclr, pencolor = clr} dir pos = maybe
	(drawCharacter f fclr clr (makeShape ts dir pos) (pensize ts))
	(drawCharacterAndLine f fclr clr (makeShape ts dir pos) (pensize ts) pos)
