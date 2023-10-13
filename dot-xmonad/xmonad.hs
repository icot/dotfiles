import XMonad

import Xmonad.Util.EZConfig
import XMonad.Uitl.Ungrab

import XMonad.Hooks.ManageDocks
import XMonad.Layout.Tabbed
import XMonad.Layout.NoBorders
import XMonad.Layout.ShowWName
import XMonad.Layout.Gaps
import XMonad.Layout.Grid
import XMonad.Actions.CycleWS
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.Loggers
import XMonad.Util.Font
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import System.IO

-- Java GUI Problems
import XMonad.Hooks.SetWMName
-- Tint2
import XMonad.Hooks.EwmhDesktops

-- Colors
fgColor = "#eee8d5"
fgInactive = "#b58900"
fgActive = "#dc322f"
fgEmpty = "#93a1a1"
bgColor = "#002b36"

-- My Manage Hook
myManageHook = composeAll
        [ className =? "Firefox" --> doShift "1:Web"
        , className =? "Thunderbird" --> doShift "9:Mail"
        , className =? "Vlc" --> doShift "3:VLC"
        , className =? "Xmessage" --> doFloat
        , className =? "Xfce4-notifyd" --> doIgnore
        , manageDocks
        ]

trayer = "trayer --transparent true --alpha 0 --tint 0x1d1f21 --widthtype pixels --width 300  --height 20 --edge top --align right"
myApps = 
    [
    trayer
    , "xscreensaver"
    , "xmodmap ~/.speedswapper"
    , "feh --bg-center --bg-fill ~/Documents/wallpapers/gits3.jpg"
    , "dropbox start"
    ]

main = do
    mapM spawnPipe myApps
    statusbar <- spawnPipe "xmobar"
    xmonad $ docks $ ewmh defaultConfig
            { workspaces = ["1:Web","2:Cons","3:VLC","4:","5:","6:","7:","8:Torrent","9:Mail"]
            , terminal = "urxvt"
            , manageHook = myManageHook <+> manageDocks <+> manageHook defaultConfig
            , layoutHook = showWName' mySWNConfig myLayout
            --, logHook = ewmhDesktopsLogHook
            , logHook = dynamicLogWithPP $ defaultPP { ppOutput = hPutStrLn statusbar }
            -- keys
            , modMask = mod4Mask
            , keys = \c -> myKeys c `M.union` keys defaultConfig c
            , borderWidth = 1
            , normalBorderColor = "#000000"--"#aa99aa"
            , focusedBorderColor = "#10aa10"
            , focusFollowsMouse = False
            -- Java GUI problems
            , startupHook = setWMName "LG3D"
            }

mySWNConfig = defaultSWNConfig
        { swn_font = "-misc-fixed-*-*-*-*-12-*-*-*-*-*-*-*"
        , swn_color = "white"
        , swn_fade = 1/4 }

myLayout = avoidStruts $ smartBorders $ (normalTiled ||| Mirror normalTiled 
        ||| myTabbed ||| myGrid )
        -- ||| myTabbed ||| myGrid ||| my3col )
    where 
        normalTiled = Tall 1 (2/100) (1/2)
        myTabbed = tabbed shrinkText myTabConfig
        myGrid = Grid
        --my3col = ThreeCol 1 (3/100) (1/2)

myTabConfig = defaultTheme
        { activeColor = "#3cb371"
        , activeTextColor = "#00ff00"
        , activeBorderColor = "3cb371" --"#aaFFaa"
        , inactiveColor = "6a6a6a"
        , inactiveTextColor = "#777777"
        , inactiveBorderColor = "#6a6a6a" --"#FF0000"
        , decoHeight = 12 }

myKeys conf@(XConfig {modMask = modm}) = 
    M.fromList $
        [ ((modm, xK_r), spawn "dmenu_run")
        , ((modm, xK_s), spawn "urxvt -e tmux")
        , ((modm, xK_t), spawn "urxvt")
        , ((modm, xK_f), spawn "firefox")
        , ((modm, xK_g), spawn "gvim")
        , ((controlMask .|. shiftMask , xK_l), spawn "i3lock -c 101010")
        , ((modm, xK_b), do
            sendMessage ToggleStruts
            sendMessage ToggleGaps)
        , ((controlMask, xK_Left), prevWS)
        , ((controlMask, xK_Right), nextWS)
        , ((controlMask, xK_Down), windows W.focusDown)
        , ((controlMask, xK_Up), windows W.focusUp)
        , ((modm, xK_Right), nextScreen)
        , ((modm, xK_Left), prevScreen)
        , ((modm, xK_Tab), toggleWS)
        ]
