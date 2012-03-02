module Graphics.X11.Turtle.XTools(
	-- * types
	Display,
	Window,
	Pixmap,
	Atom,
	Point(..),
	Position,
	Dimension,
	XEventPtr,
	XIC,
	Bufs,
	undoBuf,
	bgBuf,
	topBuf,
	GCs,
	gcForeground,
	gcBackground,
	Event(..),

	-- * open window
	forkIOX,
	openWindow,
	destroyWindow,
	closeDisplay,
	windowSize,

	-- * draws
	flush,
	getColorPixel,
	setForeground,
	copyArea,
	fillRectangle,
	fillPolygon,
	drawLineXT,
	writeStringXT,

	-- * event
	allocaXEvent,
	waitEvent,
	pending,
	nextEvent,
	getEvent,
	filterEvent,
	utf8LookupString,
	buttonPress,
	buttonRelease,
	xK_VoidSymbol,
) where

import Text.XML.YJSVG(Color(..))

import Graphics.X11(
	Display, Drawable, Window, Pixmap, Visual, Colormap, GC, Pixel, Atom,
	Point(..), Position, Dimension, XEventPtr,
	initThreads, flush, supportsLocale, setLocaleModifiers,
	connectionNumber, openDisplay, closeDisplay, internAtom,
	createSimpleWindow, destroyWindow, mapWindow, createGC, createPixmap,
	rootWindow, defaultScreen, defaultScreenOfDisplay, defaultVisual,
	defaultColormap, defaultDepth, whitePixel, blackPixel,
	copyArea, fillRectangle, drawLine, nonconvex, coordModeOrigin,
	setLineAttributes, lineSolid, capRound, joinRound, setForeground,
	allocNamedColor, color_pixel,
	allocaXEvent, pending, nextEvent,
	setWMProtocols, selectInput, button1MotionMask, buttonReleaseMask,
	buttonPressMask, keyPressMask, exposureMask,
	buttonPress, buttonRelease, xK_VoidSymbol, getGeometry)
import qualified Graphics.X11 as X(fillPolygon)
import Graphics.X11.Xlib.Extras(Event(..), getEvent)
import Graphics.X11.Xft(
	XftColor, xftDrawCreate, xftFontOpen, withXftColorValue,
	withXftColorName, xftDrawString)
import Graphics.X11.Xrender(XRenderColor(..))
import Graphics.X11.Xim(
	XIC, XNInputStyle(..), openIM, createIC, getICValue, filterEvent,
	utf8LookupString)

import Control.Monad(forM_, replicateM)
import Control.Monad.Tools(unlessM)
import Control.Concurrent(ThreadId, forkIO, threadWaitRead)
import Data.Bits((.|.), shift)
import System.Locale.SetLocale(setLocale, Category(..))
import System.Posix.Types(Fd(..))

--------------------------------------------------------------------------------

waitEvent :: Display -> IO ()
waitEvent = threadWaitRead . Fd . connectionNumber

fillPolygon :: Display -> Drawable -> GC -> [Point] -> IO ()
fillPolygon dpy win gc ps = X.fillPolygon dpy win gc ps nonconvex coordModeOrigin

forkIOX :: IO () -> IO ThreadId
forkIOX = (initThreads >>) . forkIO

data Bufs = Bufs{
	undoBuf :: Pixmap,
	bgBuf :: Pixmap,
	topBuf :: Pixmap
 }

makeBufs :: [Pixmap] -> Bufs
makeBufs [ub, bg, b] = Bufs{undoBuf = ub, bgBuf = bg, topBuf = b}
makeBufs _ = error "no such bufs"

data GCs = GCs{
	gcForeground :: GC,
	gcBackground :: GC
 }

openWindow :: IO (Display, Window, Bufs, GCs, XIC, Atom, (Dimension, Dimension))
openWindow = do
	_ <- setLocale LC_CTYPE Nothing >>= maybe (error "Can't set locale.") return
	_ <- initThreads
	unlessM supportsLocale $ error "Current locale is notSupported."
	_ <- setLocaleModifiers ""
	dpy <- openDisplay ""
	del <- internAtom dpy "WM_DELETE_WINDOW" True
	let	scr = defaultScreen dpy
	root <- rootWindow dpy scr
	(_, _, _, rWidth, rHeight, _, _) <- getGeometry dpy root
	let	black = blackPixel dpy scr
		white = whitePixel dpy scr
		depth = defaultDepth dpy scr
	bufs <- replicateM 3 $ createPixmap dpy root rWidth rHeight depth
	win <- createSimpleWindow dpy root 0 0 rWidth rHeight 1 black white
	(_, _, _, width, height, _, _) <- getGeometry dpy win
	im <- openIM dpy Nothing Nothing Nothing
	ic <- createIC im [XIMPreeditNothing, XIMStatusNothing] win
	fevent <- getICValue ic "filterEvents"
	[gc, gcBG] <- replicateM 2 $ createGC dpy win
	setForeground dpy gcBG 0xffffff
	forM_ bufs $ \bf -> fillRectangle dpy bf gcBG 0 0 rWidth rHeight
	setWMProtocols dpy win [del]
	selectInput dpy win $
		exposureMask .|. keyPressMask .|.
		buttonPressMask .|. buttonReleaseMask .|. button1MotionMask .|.
		fevent
	mapWindow dpy win
	return (dpy, win, makeBufs bufs, GCs gc gcBG, ic, del, (width, height))

getColorPixel :: Display -> Color -> IO Pixel
getColorPixel _ (RGB r g b) = return $ shift (fromIntegral r) 16 .|.
	shift (fromIntegral g) 8 .|. fromIntegral b
getColorPixel dpy (ColorName cn) = do
	let	scr = defaultScreen dpy
		colormap = defaultColormap dpy scr
	fmap (color_pixel . fst) $ allocNamedColor dpy colormap cn

withXftColor ::
	Display -> Visual -> Colormap -> Color -> (XftColor -> IO a) -> IO a
withXftColor dpy visual colormap (RGB r g b) action =
	withXftColorValue dpy visual colormap color action
	where
	color = XRenderColor {
		xrendercolor_red = fromIntegral r * 0x100,
		xrendercolor_green = fromIntegral b * 0x100,
		xrendercolor_blue = fromIntegral g * 0x100,
		xrendercolor_alpha = 0xffff
	 }
withXftColor dpy visual colormap (ColorName cn) action =
	withXftColorName dpy visual colormap cn action

writeStringBase, writeStringXT :: Display -> Pixmap -> String -> Double -> Color ->
	Position -> Position -> String -> IO ()
writeStringXT = writeStringBase
writeStringBase dpy buf fname size clr x y str = do
	let	scr = defaultScreen dpy
		scrN = defaultScreenOfDisplay dpy
		visual = defaultVisual dpy scr
		colormap = defaultColormap dpy scr
	xftDraw <- xftDrawCreate dpy buf visual colormap
	xftFont <- xftFontOpen dpy scrN $ fname ++ "-" ++ show (round size :: Int)
	withXftColor dpy visual colormap clr $ \c ->
		xftDrawString xftDraw c xftFont x y str

drawLineBase, drawLineXT :: Display -> GC -> Pixmap -> Int -> Color ->
	Position -> Position -> Position -> Position -> IO ()
drawLineXT = drawLineBase
drawLineBase dpy gc bf lw c x1 y1 x2 y2 = do
	clr <- getColorPixel dpy c
	setForeground dpy gc clr
	setLineAttributes dpy gc (fromIntegral lw) lineSolid capRound joinRound
	drawLine dpy bf gc x1 y1 x2 y2

windowSize :: Display -> Window -> IO (Dimension, Dimension)
windowSize dpy win = do
	(_, _, _, width, height, _, _) <- getGeometry dpy win
	return (width, height)
