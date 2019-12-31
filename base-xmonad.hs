{-
    Import modules
-}
import XMonad
import XMonad.Hooks.DynamicLog    -- needed to feed xmobar stdin
                                  --   (i.e. the title of the active window)
import XMonad.Hooks.EwmhDesktops  -- contains fullscreenEventHook to let
                                  --   chrome (et cetera) go fullscreen
import XMonad.Util.EZConfig       -- contains functions to
                                  --   easily configure key bindings

{-
    Solarized color scheme
-}

base03  = "#002b36"
base02  = "#073642"
base01  = "#586e75"
base00  = "#657b83"
base0   = "#839496"
base1   = "#93a1a1"
base2   = "#eee8d5"
base3   = "#fdf6e3"
yellow  = "#b58900"
orange  = "#cb4b16"
red     = "#dc322f"
magenta = "#d33682"
violet  = "#6c71c4"
blue    = "#268bd2"
cyan    = "#2aa198"
green   = "#859900"

{-
    Default font
-}

font = "-*-*-*-*-*-*-15-*-*-*-*-*-*-*"

{-
    Main
-}

main = xmonad =<< statusBar myXmobar myPP myToggleStrutsKey myConfig

{-
    Xmonad configuration
-}
myConfig = defaultConfig
             { modMask         = mod4Mask
             , handleEventHook = fullscreenEventHook
             , startupHook     = spawn "xloadimage -onroot -fullscreen " 
             } `additionalKeys` [ ( (mod4Mask,xK_p), spawn myDmenu ) ]

{-
    Command line to launch dmenu
-}

myDmenu = "dmenu_run "
            ++"-fn '"++font  ++"' "
            ++"-nb '"++base03++"' "
            ++"-nf '"++base0 ++"' "
            ++"-sb '"++base02++"' "
            ++"-sf '"++red   ++"' "

{-
    Command line to launch xmobar
-}

myXmobar = "/home/fouche/.cabal/bin/xmobar "
             ++ "-f '"++font  ++"' "
             ++ "-B '"++base03++"' "
             ++ "-F '"++base0 ++"' "
             ++ "-c '[ Run StdinReader"
             ++     ", Run Com \"date\" [\"+\\\"%F # %H.%M\\\"\"] \"date\" 600"
             ++     ", Run MultiCpu [\"-t\",\"<total0>%<total1>%\"] 10"
             ++     ", Run Battery [\"-t\",\"<left>%(<timeleft>)\"] 600"
             ++     ", Run Memory [\"-t\",\"<usedratio>%\"] 10"            
             ++     "]' "
             ++ "-t '%StdinReader%}{%multicpu% %memory% %battery% %date%'"

{-
    Formatting the feed from xmonad to xmobar (desktops, tiling, window title)
-}

myPP = xmobarPP
         { ppCurrent = xmobarColor green ""
	 , ppUrgent  = xmobarColor red   ""
	 , ppWsSep   = ""
	 , ppSep     = " "
	 , ppLayout  = ( \ x -> case x of
                                  "Full"        -> "X"
                                  "Tall"        -> "|"
                                  "Mirror Tall" -> "-"
                                  _             -> pad x
                       )
         , ppTitle   = (xmobarColor base1 base02) . (shorten 64)
         }

{-
    Sets up key to toggle struts in tiling
    (among them, the one needed for xmobar)
-}

myToggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

{- End of configuration -------------------------------------------------------}

{--
import XMonad
import XMonad.Config.Xfce
main = xmonad xfceConfig
--}