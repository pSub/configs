import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.Run (spawnPipe)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout.PerWorkspace
import System.IO

myWorkspaces =  [ "org", "www", "dev", "docs", "chat", "mail", "misc" ]

myManageHook = composeAll
                 [ isFullscreen    --> doFullFloat
                 , isDialog        --> doCenterFloat
                 , className =? "Thunderbird" --> doShift "mail"
                 , className =? ".dwb-wrapped" --> doShift "www"
                 , className =? "feh" --> doCenterFloat
                 , className =? "llpp" --> doShift "docs"
                 ] <+> manageDocks

main = do
       xmproc <- spawnPipe "xmobar ~/.xmobar/default "
       xmonad $ defaultConfig
        { startupHook = setWMName "LG3D"
        , workspaces = myWorkspaces
        , modMask = mod3Mask
        , terminal = "urxvtc"
        , manageHook = myManageHook
        , layoutHook = avoidStruts  $  layoutHook defaultConfig
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppHidden = pad
                        , ppCurrent = xmobarColor "white" "black" . pad
                        , ppHiddenNoWindows = \w -> xmobarColor "#444" "" (" " ++ w ++ " ")
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
        } `additionalKeys`  [((m .|. mod3Mask, k), windows $ f i)
         | (i, k) <- zip myWorkspaces [xK_1 .. xK_9]
         , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
