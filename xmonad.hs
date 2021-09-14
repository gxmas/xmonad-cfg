{-# LANGUAGE OverloadedStrings #-}

import XMonad
import qualified XMonad.StackSet as W

-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName (setWMName)

-- Layouts
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Renamed (Rename(Replace), renamed)
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing

-- Actions
import XMonad.Actions.FloatKeys

-- Prompt
import XMonad.Prompt
import XMonad.Prompt.AppLauncher
import XMonad.Prompt.Man

-- Util
import XMonad.Util.Cursor
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

import Data.List (intercalate)
import Data.Monoid (All)
import Data.Ratio ((%))
import GHC.IO.Handle (Handle)

import Util

main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar"

    xmonad $ def
        { modMask            = myModMask
        , terminal           = myTerminal
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , startupHook        = myStartupHook
        , layoutHook         = myLayoutHook
        , manageHook         = myManageHook
        , logHook            = myLogHook xmproc
        , handleEventHook    = myHandleEventHook
        } `additionalKeysP` myKeys

--------------------------------------------------------------------------
--  Variables
--------------------------------------------------------------------------
myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "alacritty"

myBorderWidth :: Dimension
myBorderWidth = 2

myNormalBorderColor :: String
myNormalBorderColor = "#1ba6fa"

myFocusedBorderColor :: String
myFocusedBorderColor =  "#ebebeb"

myWallpaper :: String
myWallpaper = "~/.local/share/wallpapers/weiwei-pink.jpg"

--------------------------------------------------------------------------
--  StartupHook
--------------------------------------------------------------------------
myStartupHook :: X ()
myStartupHook =
        docksStartupHook
    <+> ewmhDesktopsStartup
    <+> setDefaultCursor xC_left_ptr
    <+> spawnOnce ("xwallpaper --zoom " ++ myWallpaper)
    <+> spawnOnce "picom -b"
    <+> setWMName "LG3D"

--------------------------------------------------------------------------
--  LayoutHook
--------------------------------------------------------------------------
myLayoutHook = smartBorders
             $ avoidStruts
             $ tall ||| Mirror tall ||| Full

tall = renamed [Replace "Tall"]
     $ spacingRaw True (sqbdr 4) True (sqbdr 12) True
     $ ResizableTall 1 (3/100) (1/2) []
  where
    sqbdr n = Border n n n n

--------------------------------------------------------------------------
--  ManageHook
--------------------------------------------------------------------------
myManageHook :: ManageHook
myManageHook =
        manageDocks
    <+> namedScratchpadManageHook myScratchpads
    <+> composeAll
            [ className =? "mpv"      --> doRectFloat mpvRect
            , className =? "Xmessage" --> doCenterFloat
            , isDialog                --> doCenterFloat
            , isFullscreen            --> doFullFloat
            ]
  where
    mpvRect = W.RationalRect (1/4) (1/4) (1/2) (1/2)

--------------------------------------------------------------------------
--  LogHook
--------------------------------------------------------------------------
myLogHook :: Handle -> X ()
myLogHook h =
        dynamicLogWithPP def
            { ppOutput          = hPutStrLn h
            , ppCurrent         = xmobarColor "#98be65" "" . wrap "[" "]"
            , ppVisible         = xmobarColor "#98be65" ""
            , ppHidden          = xmobarColor "#82aaff" "" . wrap "*" ""
            , ppHiddenNoWindows = xmobarColor "#c792ea" ""
            , ppTitle           = xmobarColor "#b3afc2" "" . shorten 60
            , ppSep             = " <icon=separator.xpm/> "
            , ppUrgent          = xmobarColor "#c45500" "" . wrap "!" "!"
            , ppOrder           = \(ws:l:t:ex) -> [ws,l] ++ ex ++ [t]
            }
    <+> ewmhDesktopsLogHook

--------------------------------------------------------------------------
--  HandleEventHook
--------------------------------------------------------------------------
myHandleEventHook :: Event -> X All
myHandleEventHook =
        docksEventHook
    <+> ewmhDesktopsEventHook
    <+> fullscreenEventHook

--------------------------------------------------------------------------
--  NamedScratchpads
--------------------------------------------------------------------------
myScratchpads :: [NamedScratchpad]
myScratchpads =
    [ NS "term" nspTermCmd nspTermQuery nspTermHook ]
  where
    nspTermCmd   = intercalate " "
        [ myTerminal
        , "--class=xmonad-ns-alacritty,xmonad-ns-alacritty"
        , "--option=window.dimensions.columns=125"
        , "--option=window.dimensions.lines=50"
        ]
    nspTermQuery = className =? "xmonad-ns-alacritty"
    nspTermHook  = doCenterFloat

--------------------------------------------------------------------------
-- Prompt
--------------------------------------------------------------------------
myXPConfig :: XPConfig
myXPConfig = def
    { font              = "xft:Fira Code:Bold:pixelsize=24"
    , borderColor       = "#1ba6fa"
    , promptBorderWidth = 2
    , position          = CenteredAt { xpCenterY = 0.2, xpWidth = 0.50 }
    , alwaysHighlight   = True
    , height            = 75
    , promptKeymap      = vimLikeXPKeymap
    }

prompt :: String -> XPConfig
prompt p = myXPConfig { defaultPrompter = \_ -> p }

promptWithArg :: String -> String ->XPConfig
promptWithArg p a = myXPConfig
    { defaultText = a
    , defaultPrompter = \_ -> p
    }

launchWithNoPrompt :: Application -> X ()
launchWithNoPrompt = spawn

launchWithPrompt :: Application -> X ()
launchWithPrompt app =
  case lookup app prompts of
      Just (p, c)  -> launchApp' p app c
      Nothing      -> pure ()
  where
    prompts =
        [ ("librewolf --kiosk --search", ((prompt "Search Web: "), quote))
        , ("code",                       ((promptWithArg "VS Code: " "~/Projects/"), id))
        , ("mpv",                        ((promptWithArg "mpv: " "~/Videos/Films/"), id))
        ]

--------------------------------------------------------------------------
--  Key Bindings
--------------------------------------------------------------------------
myKeys :: [(String, X ())]
myKeys =
    [ ("M-b",                      sendMessage ToggleStruts >> sendXMobar xmobarToggle)
    , ("M-S-h",                    sendMessage MirrorExpand)
    , ("M-S-l",                    sendMessage MirrorShrink)

    , ("M-p",                      spawn "dmenu_run -fn \"Fira Code:Bold:pixelsize=24\"")

      -- Spacing
    , ("M-S-]",                    incWindowSpacing 4)
    , ("M-S-[",                    decWindowSpacing 4)
    , ("M-C-]",                    incScreenSpacing 4)
    , ("M-C-[",                    decScreenSpacing 4)

      -- Move/Resize floating windows
    , ("M-d",                      withFocused (keysResizeWindow (-10,-10) (1,1)))
    , ("M-s",                      withFocused (keysResizeWindow (10,10) (1,1)))
    , ("M-S-d",                    withFocused (keysResizeWindow (-10,-10) (3840,2160)))
    , ("M-S-s",                    withFocused (keysResizeWindow (10,10) (3840,2160)))

    , ("M-a",                      withFocused (keysResizeWindow (1920,1080) (1%2,1%2)))

      -- Apps
    , ("M-r b",                    launchWithNoPrompt "librewolf")
    , ("M-r c",                    launchWithPrompt "code")
    , ("M-r s",                    launchWithPrompt "librewolf --kiosk --search")
    , ("M-r v",                    launchWithPrompt "mpv")
    , ("M-r m",                    manPrompt myXPConfig)

      -- Scratchpads
    , ("M-C-<Return>",             namedScratchpadAction myScratchpads "term")

    -- Multimedia keys
    , ("<XF86AudioMute>",          spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
    , ("<XF86AudioLowerVolume>",   spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
    , ("<XF86AudioRaiseVolume>",   spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")
    , ("<XF86MonBrightnessUp>",    spawn "xbacklight -inc 10")
    , ("<XF86MonBrightnessDown>",  spawn "xbacklight -dec 10")
    ]
