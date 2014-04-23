import           System.IO
import           XMonad
import           XMonad.Actions.CycleWS
import           XMonad.Actions.UpdatePointer
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.UrgencyHook
import           XMonad.Layout.PerWorkspace
import qualified XMonad.StackSet              as W
import           XMonad.Util.Cursor
import           XMonad.Util.EZConfig
import           XMonad.Util.Run              (spawnPipe)

myWorkspaces = clickable . (map xmobarEscape) $ [ "org", "www", "dev₁", "dev₂", "docs", "chat", "mail" ]
             where clickable l = [ "<action=xdotool key alt+" ++ show n ++ ">" ++ ws ++ "</action>" | (n, ws) <- zip ([1..4] ++ [8,9,0]) l ]
                   xmobarEscape = concatMap doubleLts
                   doubleLts '<' = "<<"
                   doubleLts x = [x]

myManageHook = composeAll
                 [ isFullscreen    --> doFullFloat
                 , isDialog        --> doCenterFloat
                 , className =? "Thunderbird" --> doShift (myWorkspaces !! 6)
                 , className =? ".dwb-wrapped" --> doShift (myWorkspaces !! 1)
                 , className =? "feh" --> doCenterFloat
                 , className =? "llpp" --> doShift (myWorkspaces !! 4)
                 , className =? "Pinentry-gtk-2" --> doCenterFloat
                 , className =? "MPlayer" --> doCenterFloat
                 , title =? "Agenda Frame" --> doShift (myWorkspaces !! 0)
                 , title =? "floatwin" --> doCenterFloat
                 ] <+> manageDocks

myKeys = [ ("M-<Tab>", toggleWS)
         , ("M-C-<Return>", spawn "urxvtc -T floatwin")
         ]

main = do
       xmproc <- spawnPipe "xmobar ~/.xmobar/default "
       xmonad $ withUrgencyHook NoUrgencyHook defaultConfig
        { startupHook = setWMName "LG3D" >> setDefaultCursor xC_left_ptr
        , workspaces = myWorkspaces
        , modMask = mod3Mask
        , terminal = "urxvtc"
        , normalBorderColor = "black"
        , focusedBorderColor = "#f5a400"
        , manageHook = myManageHook
        , layoutHook = avoidStruts $ layoutHook defaultConfig
        , logHook = do
                     updatePointer (Relative 0.5 0.5)
                     dynamicLogWithPP xmobarPP
                       { ppOutput = hPutStrLn xmproc
                       , ppHidden = pad
                       , ppCurrent = xmobarColor "#f5a400" "black" . pad
                       , ppHiddenNoWindows = \w -> xmobarColor "#444" "" (" " ++ w ++ " ")
                       , ppTitle = xmobarColor "green" "" . shorten 50
                       , ppUrgent = xmobarColor "red" ""
                       }
        }
        `additionalKeys` [((m .|. mod3Mask, k), windows $ f i)
                         | (i, k) <- zip myWorkspaces $ [xK_1 .. xK_4] ++ [xK_8, xK_9, xK_0]
                         , (f, m) <- [(W.view, 0), (W.shift, controlMask)]]
        `removeKeys` [(mod3Mask, k) | k <- [xK_5 .. xK_7]]
        `additionalKeysP` myKeys
