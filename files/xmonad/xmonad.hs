import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Tabbed
import XMonad.Layout.NoBorders
import XMonad.Layout.ShowWName
import XMonad.Layout.Gaps
import XMonad.Layout.Grid
import XMonad.Layout.ThreeColumns
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
        [ className =? "Iceweasel" --> doShift "1:Web"
        , className =? "Icedove" --> doShift "9:Mail"
        , className =? "Vlc" --> doShift "3:VLC"
        , className =? "Xmessage" --> doFloat
        , manageDocks
        ]

myApps = 
    [ "~/bin/monitor"
    , "urxvtd"
    , "tint2"
    , "xscreensaver"
    , "xfce4-power-manager"
    , "nm-applet"
    , "feh --bg-center ~/Documents/wallpapers/kanagawa.jpg"
    , "./dropbox-dist/dropboxd"
    , "conky"
    , "volti"
    ]

main = do
    mapM spawnPipe myApps
    xmonad $ ewmh defaultConfig
            { workspaces = ["1:Web","2:Cons","3:VLC","4:","5:","6:","7:","8:Torrent","9:Mail"]
            , terminal = "urxvtc -tr"
            , manageHook = myManageHook <+> manageDocks <+> manageHook defaultConfig
            , layoutHook = showWName' mySWNConfig myLayout
            , logHook = ewmhDesktopsLogHook
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
        ||| myTabbed ||| myGrid ||| my3col )
    where 
        normalTiled = Tall 1 (2/100) (1/2)
        myTabbed = tabbed shrinkText myTabConfig
        myGrid = Grid
        my3col = ThreeCol 1 (3/100) (1/2)

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
        [ ((modm, xK_r), spawn "xfrun4")
        , ((modm, xK_s), spawn "urxvtc -e byobu")
        , ((modm, xK_f), spawn "iceweasel")
        , ((modm, xK_m), spawn "icedove")
        , ((modm, xK_v), spawn "vlc")
        , ((modm, xK_g), spawn "gvim")
        , ((modm, xK_b), do
            sendMessage ToggleStruts
            sendMessage ToggleGaps)
        , ((shiftMask, xK_h), prevWS)
        , ((shiftMask, xK_l), nextWS)
        , ((modm .|. shiftMask , xK_Right), nextScreen)
        , ((modm .|. shiftMask , xK_Left), prevScreen)
        , ((modm .|. shiftMask , xK_o), shiftNextScreen)
        , ((modm, xK_Tab), toggleWS)
        ]
