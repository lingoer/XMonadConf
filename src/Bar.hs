{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Bar (startTopBar) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar)
import Control.Monad (forever)
import Data.Bits ((.|.))
import Foreign (Ptr)
import Foreign.C as FC
import Graphics.Rendering.Cairo hiding (height, width)
import qualified Graphics.Rendering.Cairo.Internal as CI hiding (selectFontFace, setSourceRGB)
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras as Xx
import Numeric (readHex)
import qualified Themes as T

data Topbar = Topbar Display Window

foreign import ccall "cairo/cairo-xlib.h cairo_xlib_surface_create"
  c_cairo_xlib_surface_create :: Display -> Window -> Ptr Visual -> CInt -> CInt -> IO (Ptr Surface)

foreign import ccall "cairo/cairo.h cairo_surface_destroy"
  c_cairo_surface_destroy :: Ptr Surface -> IO ()

foreign import ccall "X11/Xlib.h XDefaultVisual"
  c_defaultVisual :: Display -> CInt -> IO (Ptr Visual)

-- | Start the top bar
startTopBar :: IO (MVar String)
startTopBar = do
  mvar <- newEmptyMVar
  topbar <- createTopbar
  _ <- forkIO $ forever $ do
    info <- takeMVar mvar
    drawTopbarCairo topbar info
  return mvar

-- Create a top bar using X11
createTopbar :: IO Topbar
createTopbar = do
  dpy <- openDisplay ""
  let scr = defaultScreen dpy
  rootw <- rootWindow dpy scr
  let width = fromIntegral $ displayWidth dpy scr
      height = 30
      depth = defaultDepth dpy scr
      visual = defaultVisual dpy scr
      valuemasks = cWOverrideRedirect .|. cWEventMask
  attributes <- allocaSetWindowAttributes $ \attributes -> do
    set_override_redirect attributes True
    set_event_mask attributes (exposureMask .|. keyPressMask)
    return attributes
  topBar <- createWindow dpy rootw 0 0 width height 0 depth inputOutput visual valuemasks attributes
  backgroundColorPixel <- colorPixel dpy T.backgroundColor
  setWindowBackground dpy topBar backgroundColorPixel
  let mkatom n = internAtom dpy n False
  wtype <- mkatom "_NET_WM_WINDOW_TYPE"
  dock <- mkatom "_NET_WM_WINDOW_TYPE_DOCK"
  card <- mkatom "CARDINAL"
  pstrut <- mkatom "_NET_WM_STRUT_PARTIAL"
  strut <- mkatom "_NET_WM_STRUT"
  atom <- mkatom "ATOM"
  desktop <- mkatom "_NET_WM_DESKTOP"
  setTextProperty dpy topBar "Bar" wM_NAME
  setTextProperty dpy topBar "Bar" wM_CLASS
  changeProperty32 dpy topBar wtype atom propModeReplace [fromIntegral dock]
  changeProperty32 dpy topBar pstrut card propModeReplace [0, 0, fromIntegral height, 0, 0, 0, 0, 0, 0, fromIntegral width, 0, 0]
  changeProperty32 dpy topBar strut card propModeReplace [0, 0, fromIntegral height, 0]
  changeProperty32 dpy topBar desktop card propModeReplace [0xFFFFFFFF]
  mapWindow dpy topBar
  return $ Topbar dpy topBar

drawTopbarCairo :: Topbar -> String -> IO ()
drawTopbarCairo (Topbar dpy win) info = do
  let scr = defaultScreen dpy
  visual <- c_defaultVisual dpy (fromIntegral scr)
  let width = displayWidth dpy scr
      height = 30
  surfacePtr <- c_cairo_xlib_surface_create dpy win visual width height
  surface <- CI.mkSurface surfacePtr
  renderWith surface $ do
    -- Clear the surface

    let (r, g, b) = hexToRGB T.backgroundColor -- 从 Themes.backgroundColor 中读取颜色并转换为 RGB
    setSourceRGB r g b
    paint
    -- Set the text color
    let (r', g', b') = hexToRGB T.foregroundColor -- 从 Themes.backgroundColor 中读取颜色并转换为 RGB
    setSourceRGB r' g' b'
    selectFontFace ("Sans" :: String) FontSlantNormal FontWeightNormal
    setFontSize 14
    -- Draw the text
    moveTo 5 20
    showText info
  -- Flush the display to ensure the drawing is shown
  flush dpy
  c_cairo_surface_destroy surfacePtr

hexToRGB :: String -> (Double, Double, Double)
hexToRGB ('#' : hex) =
  let (r, g, b) = case length hex of
        6 -> (take 2 hex, take 2 (drop 2 hex), drop 4 hex)
        _ -> error "Invalid color format"
      parseHex :: String -> Integer
      parseHex h = case readHex h of
        [(val, "")] -> val
        _ -> error "Invalid hex value"
      rgbValues = map parseHex [r, g, b]
   in case rgbValues of
        [r', g', b'] -> (fromIntegral r' / 255, fromIntegral g' / 255, fromIntegral b' / 255)
        _ -> error "Invalid hex value"
hexToRGB _ = error "Invalid color format"

-- surfacePtr <- c_cairo_xlib_surface_create (Ptr dpy) win visual (fromIntegral width) (fromIntegral height)
-- surface <- mkSurface surfacePtr

-- Destroy the surface to free resources

-- Draw the top bar
drawTopbar :: Topbar -> String -> IO ()
drawTopbar (Topbar dpy win) info = do
  -- Create a graphics context
  gc <- createGC dpy win
  -- Clear the window
  clearWindow dpy win
  foregroundPixel <- colorPixel dpy T.foregroundColor
  setForeground dpy gc foregroundPixel
  drawString dpy win gc 5 20 info
  -- Set the font in the GC
  -- Free the graphics context
  freeGC dpy gc
  -- Flush the display to ensure the drawing is shown
  flush dpy

colorPixel :: Display -> String -> IO Pixel
colorPixel dpy color = do
  let colormap = defaultColormap dpy (defaultScreen dpy)
  (apros, _) <- allocNamedColor dpy colormap color
  return $ color_pixel apros
