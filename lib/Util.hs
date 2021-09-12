{-# LANGUAGE OverloadedStrings #-}

module Util
    ( -- * XMobar
      sendXMobar
    , xmobarToggle
      -- * Utilities
    , quote
    ) where

import XMonad
import DBus
import DBus.Client

--------------------------------------------------------------------------
--  XMobar utility functions
--------------------------------------------------------------------------
sendXMobar :: MethodCall -> X ()
sendXMobar mc = catchIO $ connectSession >>= \c -> callNoReply c mc

xmobarMethodCall :: Variant -> MethodCall
xmobarMethodCall v =
    let mc = methodCall "/org/Xmobar/Control" "org.Xmobar.Control" "SendSignal"
    in mc { methodCallDestination = Just "org.Xmobar.Control"
          , methodCallBody = [v]
          }

xmobarToggle :: MethodCall
xmobarToggle = xmobarMethodCall (toVariant ("Toggle 0" :: String))

--------------------------------------------------------------------------
--  Utility functions
--------------------------------------------------------------------------
quote :: String -> String
quote s = "\"" ++ s ++ "\""