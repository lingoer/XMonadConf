module Main (main) where

import Bar (mBar, startTopBar)
import Keymaps
import Startup
import Themes as T
import TopBar
import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders

main :: IO ()
main = do
  mvar <- startTopBar
  xmonad . myKeys . myStartup . mBar mvar $
    desktopConfig
      { modMask = mod4Mask,
        focusedBorderColor = T.primaryColor,
        normalBorderColor = T.backgroundColor,
        borderWidth = 2,
        terminal = "kitty -1",
        layoutHook = avoidStruts (smartBorders $ Tall 1 0.03 0.62) ||| noBorders Full,
        focusFollowsMouse = False
      }
