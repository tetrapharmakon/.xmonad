{-# LANGUAGE LambdaCase #-}

-- Import modules
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.EZConfig

-- solarized
background = "#f9f5d7"

foreground = "#3c3836"

color1 = "#cc241d"

color9 = "#9d0006"

color2 = "#98971a"

base03 = "#002b36"

base02 = "#073642"

base01 = "#586e75"

base00 = "#657b83"

base0 = "#839496"

base1 = "#93a1a1"

base2 = "#eee8d5"

base3 = "#fdf6e3"

yellow = "#b58900"

orange = "#cb4b16"

red = "#dc322f"

magenta = "#d33682"

violet = "#6c71c4"

blue = "#268bd2"

cyan = "#2aa198"

green = "#859900"

-- Default font
font = "xft:DejaVu Sans Mono:size=14:antialias=true"

-- Main
main = xmonad =<< statusBar myXmobar myPP myToggleStrutsKey myConfig

-- Xmonad configuration
myConfig =
  def
    { modMask = mod4Mask
    , handleEventHook = fullscreenEventHook
    , terminal = "xfce4-terminal -e 'bash -i -c fish'"
    , startupHook = spawn "xloadimage -onroot -fullscreen ~/.xmonad/bg.jpg"
    } `additionalKeys`
  [((mod4Mask, xK_p), spawn myDmenu)]

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
  background ++
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
        \case
          "Full" -> "\9632" -- ■
          "Tall" -> "\9703" -- ◧
          "Mirror Tall" -> "\11026" -- ⬒
          _ -> "T"
    , ppTitle = xmobarColor background foreground . shorten 64
    }

-- Sets up key to toggle struts in tiling
-- (among them, the one needed for xmobar)
myToggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)
