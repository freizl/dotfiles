{-# LANGUAGE FlexibleContexts, Rank2Types #-}
-- My xmonad configuration file
module Main where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid (appEndo)

import XMonad
import XMonad.Actions.CycleWS (nextWS, prevWS, shiftToNext, shiftToPrev)
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, defaultPP, PP (..))
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks (avoidStruts, manageDocks, ToggleStruts (..))
import XMonad.Hooks.ManageHelpers (doFullFloat, doRectFloat)
import XMonad.Layout.Circle (Circle (..))
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.SimplestFloat (simplestFloat)
import XMonad.StackSet (RationalRect (..), currentTag)

main :: IO ()
main = xmonad $ ewmh $ defaultConfig
    { terminal           = "gnome-terminal"
    , modMask            = mod4Mask
    , manageHook         = myManageHook
    , layoutHook         = myLayoutHook
    , workspaces         = ["Work", "View", "Float", "Play"]
    , borderWidth        = 2
    , normalBorderColor  = "#22222"
    , focusedBorderColor = "#688cb3"
    , keys               = \c -> myKeys c `M.union` keys defaultConfig c
    }

-- | Layout hook
myLayoutHook = smartBorders $ avoidStruts
    $ onWorkspace "Work" (Mirror tiled ||| tiled ||| Full)
    $ onWorkspace "View" (Mirror tiled ||| tiled ||| Full)
    $ onWorkspace "Play" (Mirror tiled ||| tiled ||| Circle)
    $ onWorkspace "Float" simplestFloat
    $ Full
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1
    delta = 3/100
    ratio = 1/2

-- | Manage hook
myManageHook :: ManageHook
myManageHook =   manageDocks <+> manageHook defaultConfig
             <+> (onFloatingWorkSpace --> doFloat)

-- | Check if a window is on a floating space
onFloatingWorkSpace :: Query Bool
onFloatingWorkSpace = liftX $
    withWindowSet (return . (`elem` floating) . currentTag)
  where
    floating = ["Float"]

-- A list of custom keys
myKeys :: XConfig Layout -> Map (ButtonMask, KeySym) (X ())
myKeys (XConfig {modMask = myModMask}) = M.fromList $
    [ -- browser and file manager with FX
      ((myModMask, xK_F1), spawn "google-chrome")
    , ((myModMask, xK_F2), spawn "nautilus")
    , ((myModMask, xK_F3), spawn "firefox")
    , ((myModMask, xK_F4), spawn "emacs")      

      -- Logout
    -- , ((myModMask .|. shiftMask, xK_q), spawn "gnome-session-quit")
    , ((myModMask .|. shiftMask, xK_l), spawn "xscreensaver-command --lock")

      -- launcher keys
    , ((myModMask, xK_p), spawn "gmrun")
    , ((myModMask .|. shiftMask, xK_p), spawn "dmenu")

      -- Toggle struts
    , ((myModMask, xK_a), sendMessage ToggleStruts)

      -- Close window
    , ((myModMask, xK_c), kill)

      -- Full float
    , ((myModMask, xK_f), fullFloatFocused)

      -- Centered rectangle float
    , ((myModMask, xK_r), rectFloatFocused)

      -- Next & previous workspace
    , ((myModMask, xK_Left), prevWS)
    , ((myModMask, xK_Right), nextWS)
    , ((myModMask .|. shiftMask, xK_Left), shiftToPrev >> prevWS)
    , ((myModMask .|. shiftMask, xK_Right), shiftToNext >> nextWS)

      -- XF86AudioLowerVolume
    , ((0, 0x1008ff11), spawn "amixer set Master 10%-")
      -- XF86AudioRaiseVolume
    , ((0, 0x1008ff13), spawn "amixer set Master 10%+")
      -- XF86AudioMute
    , ((0, 0x1008ff12), spawn "amixer set Master toggle")
      -- XF86AudioNext
    , ((0, 0x1008ff17), spawn "exaile -n")
      -- XF86AudioPrev
    , ((0, 0x1008ff16), spawn "exaile -p")
      -- XF86AudioPlay
    , ((0, 0x1008ff14), spawn "exaile -t")
      -- XF86AudioStop
    , ((0, 0x1008ff15), spawn "exaile -s")
    ]
  where
    -- Function to fullFloat a window
    fullFloatFocused = withFocused $ \f -> windows =<< appEndo `fmap` runQuery
                          doFullFloat f

    -- Function to rectFloat a window
    rectFloatFocused = withFocused $ \f -> windows =<< appEndo `fmap` runQuery
                          (doRectFloat $ RationalRect 0.02 0.05 0.96 0.9) f
