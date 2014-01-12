import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.Run (spawnPipe)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
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
                 , className =? "Pinentry-gtk-2" --> doCenterFloat
                 , className =? "MPlayer" --> doCenterFloat
                 ] <+> manageDocks

main = do
       xmproc <- spawnPipe "xmobar ~/.xmobar/default "
       xmonad $ withUrgencyHook NoUrgencyHook defaultConfig
        { startupHook = setWMName "LG3D"
        , workspaces = myWorkspaces
        , modMask = mod3Mask
        , terminal = "urxvtc"
        , manageHook = myManageHook
        , layoutHook = avoidStruts  $  layoutHook defaultConfig
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppHidden = pad
                        , ppCurrent = xmobarColor "#f5a400" "black" . pad
                        , ppHiddenNoWindows = \w -> xmobarColor "#444" "" (" " ++ w ++ " ")
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
        } `additionalKeys`  [((m .|. mod3Mask, k), windows $ f i)
         | (i, k) <- zip myWorkspaces [xK_1 .. xK_9]
         , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
