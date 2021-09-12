{-# LANGUAGE OverloadedStrings #-}

import XMonad

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

-- Util
import XMonad.Util.Cursor
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

import Data.List (intercalate)
import Data.Monoid (All)
import Data.Ratio ((%))
import DBus
import DBus.Client
import GHC.IO.Handle (Handle)


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
            [ className =? "mpv"      --> doFloat
            , className =? "Xmessage" --> doCenterFloat
            , isDialog                --> doCenterFloat
            , isFullscreen            --> doFullFloat
            ]

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
--  Key Bindings
--------------------------------------------------------------------------
myKeys :: [(String, X ())]
myKeys =
    [ ("M-b",                      sendMessage ToggleStruts >> sendXmobar xmobarToggle)
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

      -- Scratchpads
    , ("M-C-<Return>",             namedScratchpadAction myScratchpads "term")

    -- Multimedia keys
    , ("<XF86AudioMute>",          spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
    , ("<XF86AudioLowerVolume>",   spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
    , ("<XF86AudioRaiseVolume>",   spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")
    , ("<XF86MonBrightnessUp>",    spawn "xbacklight -inc 10")
    , ("<XF86MonBrightnessDown>",  spawn "xbacklight -dec 10")
    ]

--------------------------------------------------------------------------
--  Utility functions
--------------------------------------------------------------------------
sendXmobar :: MethodCall -> X ()
sendXmobar mc = catchIO $ connectSession >>= \c -> callNoReply c mc

xmobarToggle :: MethodCall
xmobarToggle = xmobarMethodCall (toVariant ("Toggle 0" :: String))

xmobarMethodCall :: Variant -> MethodCall
xmobarMethodCall v =
    let mc = methodCall "/org/Xmobar/Control" "org.Xmobar.Control" "SendSignal"
    in mc { methodCallDestination = Just "org.Xmobar.Control"
          , methodCallBody = [v]
          }