module Keymaps (myKeys) where

import XMonad
import XMonad.Util.EZConfig

myKeys :: XConfig l -> XConfig l
myKeys =
  flip
    additionalKeysP
    [ ("M-p", spawn "rofi -show drun -m -1"),
      ("M-q", kill),
      ("M-<Return>", spawn "kitty"),
      ("M-u", spawn "xcolor | xsel -i -b"),
      ("<XF86AudioRaiseVolume>", spawn "pamixer -i 5 && pamixer --get-volume   | xargs -I {} dunstify -h 'int:value:{}' -i audio-volume-medium-symbolic Volume"),
      ("<XF86AudioLowerVolume>", spawn "pamixer -d 5 && pamixer --get-volume   | xargs -I {} dunstify -h 'int:value:{}' -i audio-volume-medium-symbolic Volume"),
      ("<XF86AudioMute>", spawn "pamixer -t   && pamixer --get-volume   | xargs -I {} dunstify -h 'int:value:{}' -i audio-volume-medium-symbolic Volume"),
      ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 5 && xbacklight -get   | xargs -I {} dunstify -h 'int:value:{}' -i display-brightness-symbolic Brightness"),
      ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 5 && xbacklight -get   | xargs -I {} dunstify -h 'int:value:{}' -i display-brightness-symbolic Brightness")
    ]
