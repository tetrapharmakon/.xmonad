{-# LANGUAGE LambdaCase #-}

import Data.Map as M

-- Import modules
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.WallpaperSetter
import XMonad.Layout.Spacing
import XMonad.Util.EZConfig

-- solarized
background = "#f9f5d7"

foreground = "#3c3836"

color1 = "#cc241d"

color2 = "#98971a"

base03 = "#002b36"

base02 = "#073642"

base0 = "#839496"

red = "#dc322f"

violet = "#6c71c4"

blue = "#268bd2"

cyan = "#2aa198"

green = "#859900"

-- Default font
font = "xft:DejaVu Sans Mono:size=10:antialias=true"

-- Main
main = xmonad =<< statusBar myXmobar myPP myToggleStrutsKey myConfig

-- Xmonad configuration
myConfig =
  def
    { modMask = mod4Mask
    , handleEventHook = fullscreenEventHook
    , terminal = "xfce4-terminal"
    , startupHook = spawn "xloadimage -onroot -fullscreen ~/.xmonad/bg.jpg"
    , manageHook = manageDocks <+> insertPosition End Newer <+> manageHook def
    -- , layoutHook = spacing 5 $ Tall 1 (5 / 100) (1 / 2)
    , logHook =
        wallpaperSetter
          defWallpaperConf
            { wallpaperBaseDir = "~/Scaricati/"
            , wallpapers =
                defWPNames myWorkspaces <>
                WallpaperList [("1:main", WallpaperDir "1")]
            }
    , workspaces = myWorkspaces
    } `additionalKeysP`
  [ ("M-p", spawn myDmenu)
  , ("M-m", spawn "mousepad")
  , ("M-t", spawn "xfce4-terminal")
  , ("M-c", spawn "code")
  , ( "M-s"
    , spawnSelected
        def --aultGSConfig
        ["urxvt-tabbed", "code", "atom", "firefox", "opera"])
  ]

myWorkspaces =
  ["1:r ", "2:t ", "3:e ", "4:w ", "5:w ", "6:q ", "7:r ", "8:r ", "9:r "]

-- Command line to launch dmenu
myDmenu =
  "dmenu_run " ++
  "-fn '" ++
  font ++
  "' " ++
  "-nb '" ++
  base03 ++
  "' " ++
  "-nf '" ++
  base0 ++ "' " ++ "-sb '" ++ base02 ++ "' " ++ "-sf '" ++ red ++ "' "

-- Command line to launch xmobar
myXmobar =
  "/usr/bin/xmobar " ++
  "-f '" ++
  font ++
  "' " ++
  "-B '" ++
  foreground ++
  "' " ++
  "-F '" ++
  cyan ++
  "' " ++
  "-c '[ Run StdinReader" ++
  ", Run Com \"date\" [\"+\\\"%F # %H.%M\\\"\"] \"date\" 600" ++
  ", Run MultiCpu [\"-t\",\"<total0>%<total1>%\"] 10" ++
  ", Run Battery [\"-t\",\"<left>%(<timeleft>)\"] 600" ++
  ", Run Memory [\"-t\",\"<usedratio>%\"] 10" ++
  "]' " ++ "-t '%StdinReader%}{%multicpu% %memory% %battery% %date%'"

-- Formatting the feed from xmonad to xmobar (desktops, tiling, window title)
myPP =
  xmobarPP
    { ppCurrent = xmobarColor color2 ""
    , ppUrgent = xmobarColor color1 ""
    , ppWsSep = ""
    , ppSep = " "
    , ppLayout =
        \x ->
          case x of
            "Full" -> "X"
            "Tall" -> "|"
            "Mirror Tall" -> "-"
            _ -> pad x
    , ppTitle = xmobarColor background foreground . shorten 64
    }

-- Sets up key to toggle struts in tiling
-- (among them, the one needed for xmobar)
myToggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)
