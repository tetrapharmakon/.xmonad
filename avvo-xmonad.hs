import Data.List
import Graphics.X11.ExtraTypes.XF86
import System.IO
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Spacing
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (spawnPipe)

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $
    docks
      defaultConfig
        { manageHook =
            manageDocks <+>
            insertPosition End Newer <+> manageHook defaultConfig
        , layoutHook =
            avoidStruts $
            spacingRaw True (Border 10 10 10 10) True (Border 10 10 10 10) True $
            layoutHook defaultConfig
        , logHook =
            dynamicLogWithPP $
            wsPP {ppOutput = hPutStrLn xmproc}
              { ppOutput = hPutStrLn xmproc
              , ppTitle = xmobarColor "green" "" . shorten 50
              }
    -- Rebind Mod to the Windows key
        , modMask = mod4Mask
    -- autostart hook, if needed
    -- , startupHook = spawn "/home/stefano/.xmonad/autostart"
        , handleEventHook = fullscreenEventHook
        , normalBorderColor = "#839496"
    -- , focusedBorderColor = "#cb4b16"
        , focusedBorderColor = "#002b36"
        } `additionalKeys`
    -- Mod-y changes the keyboard layout to US
    [ ((mod4Mask, xK_y), spawn "/home/stefano/.xmonad/layout_switch_us.sh")
    -- Mod-Y changes the keyboard layout to IT
    , ( (mod4Mask .|. shiftMask, xK_y)
      , spawn "/home/stefano/.xmonad/layout_switch_it.sh")
    -- Mod-u allows windows to cover the system bar
    , ((mod4Mask, xK_u), sendMessage ToggleStruts)
    , ( (mod4Mask, xK_p)
      , spawn "dmenu_run -fn xft:Inconsolata:size=14:antialias=true")
    , ((mod4Mask, xK_v), spawn "pavucontrol")
    , ((0, xF86XK_AudioMute), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
    , ( (0, xF86XK_AudioLowerVolume)
      , spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
    , ( (0, xF86XK_AudioRaiseVolume)
      , spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")
    ]

wsPP =
  xmobarPP
    { ppOrder = \(ws:l:t:_) -> [ws]
    , ppCurrent = xmobarColor "#2aa198" "" . wrap "[" "]"
    }
