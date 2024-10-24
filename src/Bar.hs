{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Bar (startTopBar, mBar) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (foldM_, forever)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Data.Bits ((.|.))
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Foreign (Ptr)
import Foreign.C as FC
import GHC.Generics (Generic)
import Graphics.Rendering.Cairo hiding (height, width, x, y)
import qualified Graphics.Rendering.Cairo.Internal as CI hiding (selectFontFace, setSourceRGB)
import Graphics.X11.Xlib hiding (textExtents)
import Graphics.X11.Xlib.Extras as Xx
import Numeric (readHex)
import qualified Themes as T
import XMonad (X, XConfig (logHook))
import XMonad.Hooks.DynamicLog

barHeight :: Dimension
barHeight = 24

-- Data types and instances
data ColorScheme = ColorScheme
  { foregroundColor :: String
  , backgroundColor :: String
  , text :: String
  }
  deriving (Show, Generic)

instance Data.Aeson.FromJSON ColorScheme

instance Data.Aeson.ToJSON ColorScheme

data Topbar = Topbar Display Window

-- Foreign imports
foreign import ccall "cairo/cairo-xlib.h cairo_xlib_surface_create"
  c_cairo_xlib_surface_create :: Display -> Window -> Ptr Visual -> CInt -> CInt -> IO (Ptr Surface)

foreign import ccall "cairo/cairo.h cairo_surface_destroy"
  c_cairo_surface_destroy :: Ptr Surface -> IO ()

foreign import ccall "X11/Xlib.h XDefaultVisual"
  c_defaultVisual :: Display -> CInt -> IO (Ptr Visual)

-- Main functions
mBar :: MVar String -> XConfig l -> XConfig l
mBar mvar conf =
  conf
    { logHook = barLogHook mvar
    }

barLogHook :: MVar String -> X ()
barLogHook mvar =
  dynamicLogWithPP $
    def
      { ppOutput = putMVar mvar
      , ppCurrent = barColor T.backgroundColor T.primaryColor
      , ppVisible = barColor T.backgroundColor T.secondaryColor
      , ppHidden = barColor T.primaryColor T.backgroundColor
      , ppWsSep = ","
      , ppHiddenNoWindows = barColor T.foregroundColor T.backgroundColor
      , ppOrder = \case
          (ws : _l : t : _ex) ->
            [ "["
                ++ ws
                -- ++ ","
                -- ++ barColor T.foregroundColor T.backgroundColor (" ||" ++ t)
                ++ "]" -- ws : [t]
            ]
          _any -> _any
      }
 where
  barColor :: String -> String -> String -> String
  barColor fg bg txt = unpack $ Data.Aeson.encode $ ColorScheme fg bg txt

startTopBar :: IO (MVar String)
startTopBar = do
  mvar <- newEmptyMVar
  topbar <- createTopbar
  _ <- forkIO $ forever $ do
    info <- takeMVar mvar
    drawTopbarCairo topbar info
  return mvar

createTopbar :: IO Topbar
createTopbar = do
  dpy <- openDisplay ""
  let scr = defaultScreen dpy
  rootw <- rootWindow dpy scr
  let width = fromIntegral $ displayWidth dpy scr
      height = barHeight
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
 where
  colorPixel dpy color = do
    let colormap = defaultColormap dpy (defaultScreen dpy)
    (apros, _) <- allocNamedColor dpy colormap color
    return $ color_pixel apros

drawTopbarCairo :: Topbar -> String -> IO ()
drawTopbarCairo (Topbar dpy win) info = do
  let scr = defaultScreen dpy
  visual <- c_defaultVisual dpy (fromIntegral scr)
  let width = displayWidth dpy scr
      height = barHeight
  surfacePtr <- c_cairo_xlib_surface_create dpy win visual width (fromIntegral height)
  surface <- CI.mkSurface surfacePtr
  renderWith surface $ do
    case parseColorSchemes info of
      Left err -> do
        setSourceRGB 1 0 0
        selectFontFace ("Sans" :: String) FontSlantNormal FontWeightNormal
        moveTo 5 20
        showText ("Error: " ++ err)
      Right colorSchemes -> foldM_ drawColorScheme 0 colorSchemes
  flush dpy
  c_cairo_surface_destroy surfacePtr
 where
  parseColorSchemes :: String -> Either String [ColorScheme]
  parseColorSchemes = Data.Aeson.eitherDecode . pack
  drawColorScheme x (ColorScheme fg bg txt) = do
    selectFontFace ("Sans" :: String) FontSlantNormal FontWeightNormal
    setFontSize (fromIntegral barHeight - 2)
    TextExtents _ _ _ height width _ <- textExtents txt
    let (rBg, gBg, bBg) = hexToRGB bg
    setSourceRGB rBg gBg bBg
    rectangle x 0 (fromIntegral barHeight) (fromIntegral barHeight)
    fill
    let (rFg, gFg, bFg) = hexToRGB fg
    setSourceRGB rFg gFg bFg
    moveTo (x + ((fromIntegral barHeight - width - 4) / 2)) ((fromIntegral barHeight + height) / 2)
    showText txt
    return (x + fromIntegral barHeight - 4)

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
