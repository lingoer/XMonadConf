{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Bar (startTopBar) where

import Control.Concurrent
import Control.Monad
import Data.Aeson
import Data.Bits ((.|.))
import Data.Text
import Graphics.X11.Xft
import Graphics.X11.Xinerama
import Graphics.X11.Xlib
import System.Exit
import Themes
import GHC.Generics

data TextSegment = TextSegment
  { text :: !Text
  , fgColor :: !Text
  , bgColor :: !Text
  , borderColor :: !Text
  } deriving (Show, Generic)
instance FromJSON TextSegment
instance ToJSON TextSegment

getScreenWidth :: (Num b) => Display -> IO b
getScreenWidth dpy =
  xineramaQueryScreens dpy >>= \case
    Just (_ : screen1 : _anyother) -> do
      return $ fromIntegral $ xsi_width screen1
    _anyOther -> do
      putStrLn "fail"
      exitFailure

createTopbar :: Display -> IO Window
createTopbar dpy = do
  rootWin <- rootWindow dpy screenNumber
  attributes <- allocaSetWindowAttributes $ \attributes -> do
    set_override_redirect attributes True
    set_event_mask attributes (exposureMask .|. keyPressMask)
    return attributes
  screenWidth <- getScreenWidth dpy
  topBar <- createWindow dpy rootWin 1920 0 screenWidth 24 0 depth inputOutput visual valuemasks attributes
  setWindowBackground dpy topBar 0x282c34
  mapWindow dpy topBar
  return topBar
 where
  screenNumber = defaultScreen dpy
  screen = defaultScreenOfDisplay dpy
  visual = defaultVisualOfScreen screen
  depth = defaultDepthOfScreen screen
  valuemasks = cWOverrideRedirect .|. cWEventMask

startTopBar = do
  m <- newEmptyMVar
  dpy <- openDisplay ""
  bar <- createTopbar dpy
  _ <- forkIO $ loop bar m
  return m

loop bar m = do
  newLines <- takeMVar m
  case decode newLines of
    Just (TextSegment {..}) -> do
      --drawText bar text fgColor bgColor
      loop bar m
    _anyother -> exitFailure
{--
drawText dpy win position TextSegment{..} = do
  withXftColorName dpy visual colormap fgColor $ \fgColor -> do
    withXftColorName dpy visual colormap bgColor $ \bgColor -> do
      withXftColorName dpy visual colormap borderColor $ \borderColor -> do
        withXftDraw dpy win visual depth $ \draw -> do
          withXftFontName dpy visual colormap "Hack Nerd Font Mono" $ \font -> do
            let
              x = 0
              y = 0
              width = 1920
              height = 24
              border = 0
              bordercolor = borderColor
              text = unpack text
            xftDrawRect draw bgColor x y width height
            xftDrawRect draw borderColor x y width border
            xftDrawRect draw borderColor x y border height
            xftDrawRect draw borderColor x (y + height - border) width border
            xftDrawRect draw borderColor (x + width - border) y border height
            xftDrawString draw fgColor font x y text
  where
    screen = defaultScreenOfDisplay dpy
    visual = defaultVisualOfScreen screen
    colormap = defaultColormapOfScreen screen
    depth = defaultDepthOfScreen screen
    --}
