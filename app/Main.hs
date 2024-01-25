module Main (main) where

import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import TopBar
import Startup
import Themes as T
import Keymaps

main :: IO ()
main =
  xmonad . myKeys . myStartup . myTopBar $ desktopConfig
      { modMask = mod4Mask
      , focusedBorderColor = T.primaryColor
      , normalBorderColor = T.backgroundColor
      , borderWidth = 2
      , terminal = "kitty"
      , layoutHook = avoidStruts (smartBorders $ Tall 1 0.03 0.62) ||| noBorders Full
      , focusFollowsMouse = False
      }

