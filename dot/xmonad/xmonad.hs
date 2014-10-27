import XMonad
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
--

-- My Manage Hook

myManageHook = composeAll
        [ className =? "Iceweasel" --> doShift "1:Web"
        , className =? "Icedove" --> doShift "9:Mail"
        , className =? "Vlc" --> doShift "3:VLC"
        , className =? "Xmessage" --> doFloat
        , manageDocks
        ]
        
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
    {
    ppCurrent           =   dzenColor "white" "#1B1D1E" . pad
    , ppVisible           =   dzenColor "white" "#1B1D1E" . pad
    , ppHidden            =   dzenColor "grey" "#1B1D1E" . pad
    , ppHiddenNoWindows   =   dzenColor "#7b7b7b" "#1B1D1E" . pad
    , ppUrgent            =   dzenColor "#ff0000" "#1B1D1E" . pad
    , ppWsSep             =   ""
    , ppSep               =   "  |  "
    , ppLayout            =   dzenColor "green" "#1B1D1E" .
                              (\x -> case x of
                                  "ResizableTall"             ->      "RT"
                                  "Mirror ResizableTall"      ->      "MRT"
                                  "Full"                      ->      "F"
                                  "Simple Float"              ->      "SF"
                                  _                           ->      x
                              )
--    , ppTitle             =   (" " ++) . dzenColor "green" "#1B1D1E" . dzenEscape . shorten 80
    , ppOrder = \xs -> take 2 xs ++ drop 3 xs
    , ppExtras = 
            [ 
            -- dzenColorL "grey" "#1B1D1E" fixedWidthL AlignCenter " " 60 logTitle
            fixedWidthL AlignCenter " " 60 logTitle
            , loadAvg
            , date "%Y-%m-%d %R:%S" 
            ] 
    , ppOutput            =   hPutStrLn h
    }

myXmonadBar = "dzen2 -x '0' -y '1180' -h '20' -w '1920' -ta 'l' -fg 'grey' -bg '#1B1D1E' -fn inconsolata:size=10"
myStatusBar = "conky -c ~/.xmonad/.conky_dzen | dzen2 -x '800' -y '1056' -w '640' -h '20' -ta 'r' -bg '#1B1D1E' -fg '#FFFFFF' -y '0'"
myTrayer = "killall trayer; trayer --edge top --align right --widthtype request --SetDockType true --SetPartialStrut true --transparent true --alpha 0 --tint 0x000000 --expand true --heighttype pixel --height 16"

main = do
    dzenLeftBar <- spawnPipe myXmonadBar
--    dzenRightBar <- spawnPipe myStatusBar
    xmonad $ defaultConfig
            { workspaces = ["1:Web","2:Consoles","3:","4:","5:","6:","7:","8:Misc","9:Mail"]
            , terminal = "urxvtc -tr"
            , manageHook = myManageHook <+> manageDocks <+> manageHook defaultConfig
            , layoutHook = showWName' mySWNConfig myLayout
            , logHook = myLogHook dzenLeftBar >> fadeInactiveLogHook 0xdddddddd
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

myLayout = avoidStruts $ smartBorders $ (normalTiled ||| Mirror normalTiled ||| myTabbed ||| myGrid)
    where 
        normalTiled = Tall 1 (2/100) (1/2)
        myTabbed = tabbed shrinkText myTabConfig
        myGrid = Grid

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
        , ((modm, xK_v), spawn "gvim")
        , ((modm, xK_b), do
            sendMessage ToggleStruts
            sendMessage ToggleGaps)
        , ((modm, xK_Left), prevWS)
        , ((modm, xK_Right), nextWS)
        , ((modm, xK_Down), windows W.focusDown)
        , ((modm, xK_Up), windows W.focusUp)
        , ((modm .|. shiftMask , xK_Right), nextScreen)
        , ((modm .|. shiftMask , xK_Left), prevScreen)
        , ((modm .|. shiftMask , xK_o), shiftNextScreen)
        , ((modm, xK_Tab), toggleWS)
        ]
