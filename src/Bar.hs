{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Bar (startTopBar) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar)
import Control.Monad (forever)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Graphics.X11.Xft
import Graphics.X11.Xlib hiding (drawSegments, textWidth)
import Graphics.X11.Xlib.Extras

data Topbar = Topbar {display :: Display, window :: Window}

-- | Start the top bar
startTopBar :: IO (MVar String)
startTopBar = do
  mvar <- newEmptyMVar
  topbar <- createTopbar
  _ <- forkIO $ forever $ do
    info <- takeMVar mvar
    drawTopbar topbar info
  return mvar

-- Create a top bar using X11
createTopbar :: IO Topbar
createTopbar = do
  dpy <- openDisplay ""
  let scr = defaultScreen dpy
  rootw <- rootWindow dpy scr
  let black = blackPixel dpy scr
      white = whitePixel dpy scr
      width = displayWidth dpy scr
      height = (30 :: Integer)
  win <- createSimpleWindow dpy rootw 0 0 (fromIntegral width) (fromIntegral height) 1 black white
  wM_DELETE_WINDOW <- internAtom dpy "WM_DELETE_WINDOW" False
  Graphics.X11.Xlib.setWMProtocols dpy win [wM_DELETE_WINDOW]
  setWMName dpy win "Topbar"
  Bar.setWMProtocols dpy win [wM_DELETE_WINDOW] -- Use the function here
  mapWindow dpy win
  return $ Topbar dpy win

-- Set the window name
setWMName :: Display -> Window -> String -> IO ()
setWMName dpy win name = do
  atom <- internAtom dpy "_NET_WM_NAME" False
  changeProperty32 dpy win atom cARDINAL propModeReplace (map (fromIntegral . fromEnum) name)

-- Set the WM protocols
setWMProtocols :: Display -> Window -> [Atom] -> IO ()
setWMProtocols dpy win atoms = do
  wmProtocols <- internAtom dpy "WM_PROTOCOLS" False
  changeProperty32 dpy win wmProtocols aTOM propModeReplace (map fromIntegral atoms)

-- Draw the top bar
drawTopbar :: Topbar -> String -> IO ()
drawTopbar (Topbar dpy win) info = do
  -- Create a graphics context
  gc <- createGC dpy win
  -- Clear the window
  clearWindow dpy win

  -- Parse the XMobar formatted info
  let parsedInfo = parseXMobarFormat info

  -- Draw each segment of the parsed info
  drawSegments dpy win gc 10 20 parsedInfo

  -- Free the graphics context
  freeGC dpy gc

  -- Flush the display to ensure the drawing is shown
  flush dpy

-- Helper function to parse XMobar formatted string
parseXMobarFormat :: String -> [(String, String)]
parseXMobarFormat [] = []
parseXMobarFormat str =
  let (pre, rest) = break (== '<') str
      (tag, post) = break (== '>') rest
   in if null rest
        then [(pre, "#FFFFFF")] -- Default color
        else (pre, "#FFFFFF") : parseTag (tag ++ ">") post

parseTag :: String -> String -> [(String, String)]
parseTag tag rest =
  let color = extractColorFromTag tag
      (text, remaining) = break (== '<') rest
   in (text, color) : parseXMobarFormat remaining

-- Extract color from XMobar tag
extractColorFromTag :: String -> String
extractColorFromTag tag =
  if "<fc=" `isPrefixOf` tag
    then takeWhile (/= '>') (drop 4 tag)
    else "#FFFFFF" -- Default color

-- Draw parsed segments
drawSegments :: Display -> Window -> GC -> Int -> Int -> [(String, String)] -> IO ()
drawSegments _ _ _ _ _ [] = return ()
drawSegments dpy win gc x y ((text, color) : xs) = do
  -- Set the foreground color
  pixel <- colorPixel dpy color
  setForeground dpy gc pixel

  -- Draw the text
  drawString dpy win gc (fromIntegral x) (fromIntegral y) text

  -- Calculate the new x position
  let newX = x + textWidth text

  -- Draw the next segment
  drawSegments dpy win gc newX y xs

-- Convert color string to pixel value
colorPixel :: Display -> String -> IO Pixel
colorPixel dpy color = do
  let colormap = defaultColormap dpy (defaultScreen dpy)
  (apros, _) <- allocNamedColor dpy colormap color
  return $ color_pixel apros

-- Calculate the width of the text
textWidth :: String -> Int
textWidth text = length text * 8 -- Assuming each character is 8 pixels wide
