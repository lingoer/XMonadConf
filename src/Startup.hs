module Startup (myStartup) where

import XMonad
import XMonad.Util.Cursor

myStartup :: XConfig l -> XConfig l
myStartup conf = conf{startupHook = startupHook conf <> myStartupHook}

myStartupHook :: X ()
myStartupHook = do
  setDefaultCursor xC_left_ptr
  spawn "nitrogen --restore"
  spawn "xinput set-prop 'ELAN2305:00 04F3:3122 Touchpad' 'libinput Tapping Enabled' 1"
  spawn "picom -b"
