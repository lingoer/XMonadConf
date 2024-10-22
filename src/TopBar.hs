{-# LANGUAGE LambdaCase #-}

module TopBar (myTopBar) where

-- Standard library imports
import Control.Concurrent
import Control.Monad (void)
-- XMonad imports

-- External library imports

-- Project-specific imports
import Themes
import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import Xmobar

boxed :: String -> String
boxed = wrap p n
  where
    p = "<box color=" ++ primaryColor ++ ">"
    n = "</box>"

topBarLogString :: X String
topBarLogString =
  dynamicLogString
    def
      { ppCurrent = xmobarColor backgroundColor primaryColor . wrap " " " ",
        ppVisible = xmobarColor backgroundColor secondaryColor . wrap " " " ",
        ppHidden = xmobarColor primaryColor backgroundColor . boxed . wrap " " " ",
        ppHiddenNoWindows = xmobarColor foregroundColor backgroundColor . wrap " " " ",
        ppWsSep = " ",
        ppTitle = shorten 70,
        -- , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"
        ppSep = " || ",
        -- , ppExtras = [windowCount]
        ppOrder = \case
          (ws : _l : t : _ex) -> ws : [t]
          _any -> _any
      }

myTopBar :: XConfig l -> XConfig l
myTopBar conf =
  conf
    { logHook = (topBarLogString >>= xmonadPropLog) <> logHook desktopConfig,
      startupHook = startupHook conf <> io startXmobar
    }

startXmobar :: IO ()
startXmobar =
  void . forkIO . Xmobar.xmobar $
    Xmobar.defaultConfig
      { bgColor = backgroundColor,
        fgColor = foregroundColor,
        Xmobar.font = Themes.font,
        Xmobar.allDesktops = True,
        position = TopH 24,
        template = "%UnsafeXMonadLog% } %date% { %battery%",
        commands =
          [ Run UnsafeXMonadLog,
            Run $ Date "%H:%M <raw=1: /> %b %_d " "date" 10,
            Run $ Battery ["-t", "<acstatus> <left>%", "-L", "10", "-H", "80", "--", "-O", "AC", "-o", "Bat", "-h", secondaryColor, "-l", alertColor] 10
          ]
      }
