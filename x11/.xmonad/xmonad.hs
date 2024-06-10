import           Data.List                   (isInfixOf)
import           System.IO
import           XMonad
import           XMonad.Actions.CycleWS
import           XMonad.Actions.UpdatePointer
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.UrgencyHook
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Layout.PerWorkspace
import qualified XMonad.StackSet              as W
import           XMonad.Util.Cursor
import           XMonad.Util.EZConfig
import           XMonad.Util.Run              (spawnPipe)
import qualified XMonad.Util.Hacks as Hacks

myWorkspaces = clickable . (map xmobarEscape) $ [ "org", "www", "dev₁", "dev₂", "docs", "chat", "mail" ]
             where clickable l = [ "<action=xdotool key alt+" ++ show n ++ ">" ++ ws ++ "</action>" | (n, ws) <- zip ([1..4] ++ [8,9,0]) l ]
                   xmobarEscape = concatMap doubleLts
                   doubleLts '<' = "<<"
                   doubleLts x = [x]

myManageHook = composeAll
                 [ isFullscreen    --> doFullFloat
                 , isDialog        --> doCenterFloat
                 , className =? "superProductivity" --> doShift (myWorkspaces !! 0)
                 , className =? "FFPWA-01HZZDRGJ9MS83HN6YA8K68FMG" --> doShift (myWorkspaces !! 5)
                 , className =? "Signal" --> doShift (myWorkspaces !! 5)
                 , className =? "thunderbird" --> doShift (myWorkspaces !! 6)
                 , className =? "firefox" --> doShift (myWorkspaces !! 1)
                 , className =? "Chromium-browser" --> doShift (myWorkspaces !! 1)
                 , className =? "Brave-browser" --> doShift (myWorkspaces !! 1)
                 , className =? "feh" --> doCenterFloat
                 , className =? "llpp" --> doShift (myWorkspaces !! 4)
                 , className =? "Pinentry" --> doCenterFloat
                 , className =? "MPlayer" --> doCenterFloat
                 , className =? "Eclipse" --> doShift (myWorkspaces !! 2)
                 , className =? "obsidian" --> doShift (myWorkspaces !! 0)
                 , title =? "Agenda Frame" --> doShift (myWorkspaces !! 0)
                 , title =? "floatwin" --> doCenterFloat
                 , appName =? "sun-awt-X11-XWindowPeer" <&&> className =? "jetbrains-idea" --> doIgnore
                 ] <+> manageDocks

myKeys = [ ("M-<Tab>", toggleWS)
         , ("M-C-<Return>", spawn "alacritty --title floatwin")
         , ("M-C-p", spawn "passmenu")
         , ("M-C-t", spawn "thunar")
         ]

main = do
       xmproc <- spawnPipe "xmobar ~/.xmobar/default "
       xmonad $ docks $ withUrgencyHook NoUrgencyHook $ ewmhFullscreen def
        { startupHook = setWMName "LG3D" >> setDefaultCursor xC_left_ptr
        , workspaces = myWorkspaces
        , modMask = mod3Mask
        , terminal = "alacritty"
        , normalBorderColor = "black"
        , focusedBorderColor = "#f5a400"
        , manageHook = myManageHook
        , handleEventHook = mconcat [ handleEventHook def
                          , Hacks.trayerAboveXmobarEventHook
                          ]
        , layoutHook = avoidStruts $ layoutHook def
        , logHook = do
                     updatePointer (0.5, 0.5) (0, 0)
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
