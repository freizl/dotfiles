{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types       #-}
-- | My xmonad configuration file
-- config template: http://www.haskell.org/haskellwiki/Xmonad/Config_archive/Template_xmonad.hs_%280.9%29

module Main where

import           Data.Map                    (Map)
import qualified Data.Map                    as M
import           Data.Monoid                 (appEndo)

import           System.IO
import           XMonad
import           XMonad.Actions.CycleWS      (nextWS, prevWS, shiftToNext,
                                              shiftToPrev)
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops   (ewmh)
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.Circle        (Circle (..))
import           XMonad.Layout.Gaps
import           XMonad.Layout.Grid
import           XMonad.Layout.Spacing
import           XMonad.Layout.NoBorders     (noBorders, smartBorders)
import           XMonad.Layout.PerWorkspace  (onWorkspace)
import           XMonad.Layout.SimplestFloat (simplestFloat)
import           XMonad.StackSet             (RationalRect (..), currentTag)
import           XMonad.Util.EZConfig        (additionalKeys)
import           XMonad.Util.Run             (spawnPipe)

main :: IO ()
main = do
    xmproc <- spawnPipe "/usr/bin/xmobar $HOME/.xmonad/xmobarrc"
    xmonad (ewmh (
               defaultConfig { terminal           = "gnome-terminal"
                             , modMask            = mod4Mask
                             , workspaces         = ["1:Web", "2:Edit", "3:Mail", "4:Chat", "5", "6", "7", "8", "9:Float"]
                             , layoutHook         = myLayoutHook
                             , manageHook         = myManageHook
                             , logHook            = myLogHook xmproc
                             , keys               = \c -> myKeys c `M.union` keys defaultConfig c
                             }))

-- | Workspaces
--
myWorkspaces = []

wsChat :: String
wsChat = "4:Chat"

wsWeb :: String
wsWeb = "1:Web"

wsMail :: String
wsMail = "3:Mail"

-- | TODO add space when grid layout
--   Circle
myLayoutHook = noBorders $ avoidStruts
    $ onWorkspace "1:Web" (Full ||| tiled ||| Mirror tiled)
    $ onWorkspace "3:Mail" (spacing 5 Grid ||| Full)
    $ onWorkspace "4:Chat" (Circle)
    $ onWorkspace "9:Float" simplestFloat
    $ (tiled ||| Mirror tiled ||| Full)
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1
    delta = 3/100
    ratio = 1/2
myManageHook :: ManageHook
myManageHook = manageDocks
               <+> manageHook defaultConfig
               <+> onSpecial

-- |
onSpecial = composeAll
            -- per-window options, use `xprop' to learn window names and classes
            [ title =? "xclock"     --> doFloat
            , title =? "xeyes"      --> doFloat
            , className =? "skype"  --> doShift wsChat
            ]

myLogHook xmproc = dynamicLogWithPP $ xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }

-- A list of custom keys
myKeys :: XConfig Layout -> Map (ButtonMask, KeySym) (X ())
myKeys (XConfig {modMask = myModMask}) = M.fromList $
    [ ((myModMask, xK_F1), spawn "google-chrome")
    , ((myModMask, xK_F2), spawn "firefox")
    , ((myModMask, xK_F3), spawn "emacsclient -c")
    , ((myModMask, xK_F4), spawn "thunar")
    -- , ((myModMask, xK_F8), spawn "nautilus --no-desktop --browser")

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

    ]
  where
    -- Function to fullFloat a window
    fullFloatFocused = withFocused $ \f -> windows =<< appEndo `fmap` runQuery
                          doFullFloat f

    -- Function to rectFloat a window
    rectFloatFocused = withFocused $ \f -> windows =<< appEndo `fmap` runQuery
                          (doRectFloat $ RationalRect 0.02 0.05 0.96 0.9) f
