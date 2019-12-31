import System.IO (hPutStrLn)

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.NoBorders (smartBorders)

-- import XMonad.Layout.DragPane
import XMonad.Layout.LayoutCombinators hiding ((|||))

import XMonad.Layout.Tabbed

import XMonad.Util.EZConfig
import XMonad.Util.Run (spawnPipe)

--import XMonad.Actions.SpawnOn
import qualified XMonad.StackSet as W

background = "#f9f5d7"

foreground = "#3c3836"

color0 = "#fdf4c1"

color8 = "#928374"

color1 = "#cc241d"

color9 = "#9d0006"

color2 = "#98971a"

color10 = "#79740e"

color3 = "#d79921"

color11 = "#b57614"

color4 = "#458588"

color12 = "#076678"

color5 = "#b16286"

color13 = "#8f3f71"

color6 = "#689d6a"

color14 = "#427b58"

color7 = "#7c6f64"

color15 = "#3c3836"

font = "xft:DejaVu Sans Mono:pixelsize=18"

myStdWorkspaces =
  zip [xK_1, xK_2, xK_3, xK_4, xK_5, xK_6, xK_7, xK_8, xK_9] $
  map show [1 .. 9 :: Int]

myExtraWorkspaces =
  [(xK_backslash, "Α"), (xK_0, "0"), (xK_apostrophe, "Ψ"), (xK_igrave, "Ω")]

myWorkspaces =
  [head myExtraWorkspaces] ++ myStdWorkspaces ++ tail myExtraWorkspaces

myWorkspacesFocus =
  [((mod4Mask, key), (windows $ W.greedyView ws)) | (key, ws) <- myWorkspaces]

myWorkspacesShift =
  [ ((mod4Mask .|. shiftMask, key), (windows $ W.shift ws))
  | (key, ws) <- myWorkspaces
  ]

myKeys =
  [ ((mod4Mask, xK_p), spawn myDmenu)
  , ((mod4Mask, xK_b), sendMessage ToggleStruts)
  , ((mod4Mask, xK_u), focusUrgent)
  , ((mod4Mask .|. shiftMask, xK_e), spawn "emacsclient -c")
  ] ++
  myWorkspacesFocus ++ myWorkspacesShift

main = do
  bar <- spawnPipe myXmobar
  wallpaper <- spawnPipe "xsetroot -solid black"
  xmonad $
    withUrgencyHook
      NoUrgencyHook
      defaultConfig
        { borderWidth = 1
        , workspaces = map snd myWorkspaces
        , terminal = "urxvt"
        , modMask = mod4Mask
        , logHook = myLogHook bar
        , layoutHook = myLayoutHook
        , manageHook = manageDocks <+> manageHook defaultConfig
        } `additionalKeys`
    myKeys

--                 , normalBorderColor  = background
--                 , focusedBorderColor = color9
--                 , manageHook         = manageDocks <+> manageSpawn <+> manageHook defaultConfig
--                 , startupHook = do
--                    spawnOn "1" "urxvt"
--                    spawnOn "2" "urxvt"
--                    spawnOn "3" "urxvt"
--                    spawnOn "4" "opera"
--                    spawnOn "5" "opera --new-window"
--                    spawnOn "6" "urxvt"
--                    spawnOn "7" "urxvt"
--                    spawnOn "8" "urxvt"
myDmenu =
  "dmenu_run " ++
  "-f " -- grab keyboard
   ++
  "-i " -- case insensitive
   ++
  "-fn '" ++
  font ++
  "' " ++
  "-nb '" ++
  background ++
  "' " ++
  "-nf '" ++
  foreground ++ "' " ++ "-sb '" ++ color1 ++ "' " ++ "-sf '" ++ color15 ++ "' "

myXmobar =
  "xmobar " ++
  "-f '" ++
  font ++
  "' " ++
  "-B '" ++
  background ++
  "' " ++
  "-F '" ++
  foreground ++
  "' " ++
  "-c '[ Run StdinReader" ++
  ", Run Com \"date\" [\"+%a%d%b%H%M\"] \"date\"         600" ++
  ", Run MultiCpu     [\"-t\",\"<total0>\\215<total1>\\215<total2>\\215<total3>\"] 10" ++
  ", Run DynNetwork   [\"-t\",\"<tx>\\8645<rx>\"]        10" ++
  ", Run Battery      [\"-t\",\"<left>=<timeleft>\"]     600" ++
  ", Run Memory       [\"-t\",\"<usedratio>\"]           10" ++
  ", Run Swap         [\"-t\",\"<usedratio>\"]           10" ++
  "]' " ++
  "-t '%StdinReader%}{%dynnetwork% %multicpu% %memory%+%swap% %battery% %date%'"

myLogHook h =
  dynamicLogWithPP $
  xmobarPP
    { ppCurrent = xmobarColor color2 ""
    , ppUrgent = xmobarColor color1 ""
    , ppWsSep = ""
    , ppSep = " "
    , ppLayout =
        (\x ->
           case x of
             "Full" -> "\9632" -- ■
             "Tall" -> "\9703" -- ◧
             "Mirror Tall" -> "\11026" -- ⬒
             _ -> "T"
                                  -- _             -> pad x
         )
    , ppTitle = (xmobarColor foreground background) . (shorten 120)
    , ppOutput = hPutStrLn h
    }

myLayoutHook =
  avoidStruts . smartBorders $ tiled ||| Mirror tiled ||| Full ||| gimpLayout
  where
    tiled = Tall 1 (3 / 100) (1 / 2)
    tabbedLayout = tabbedBottomAlways shrinkText defaultTheme
    gimpLayout = tabbedLayout ****||* Full
