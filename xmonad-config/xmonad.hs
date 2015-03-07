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
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.Circle        (Circle (..))
import           XMonad.Layout.Gaps
import           XMonad.Layout.Grid
import           XMonad.Layout.NoBorders     (noBorders, smartBorders)
import           XMonad.Layout.PerWorkspace  (onWorkspace)
import           XMonad.Layout.SimplestFloat (simplestFloat)
import           XMonad.Layout.Spacing
import           XMonad.StackSet             (RationalRect (..), currentTag)
import qualified XMonad.StackSet             as W
import           XMonad.Util.EZConfig        (additionalKeys, removeKeys)
import           XMonad.Util.Run             (spawnPipe)
import XMonad.Actions.UpdatePointer

-- | find proper mod key
--   `xmodmap`, `xkeycaps`, `xev`
--
main :: IO ()
main = do
    xmproc <- spawnPipe "/usr/bin/xmobar $HOME/.xmonad/xmobarrc"
    xmonad
      $ ewmh
      $ clearConflictKeys
      $ defaultConfig { terminal           = "gnome-terminal"
                         -- terminal           = "urxvtc -pe tabbed"
                      , modMask            = mod4Mask
                      , workspaces         = myWorkspaces
                      , layoutHook         = myLayoutHook
                      , manageHook         = myManageHook
                      , logHook            = myLogHook xmproc >> setWMName "LG3D" >> updatePointer (Relative 1 1)
                      , keys               = \c -> myKeys c `M.union` keys defaultConfig c
                      }

-- | Workspaces
--
myWorkspaces :: [String]
myWorkspaces = [wsEdit, wsTerm, wsChat, wsTest] ++ map show ([4..9] :: [Int])

wsEdit, wsTerm, wsChat, wsTest :: String
wsEdit = "1:Edit"
wsTerm = "2:Term"
wsChat = "3:Chat"
wsTest = "4:Test"


myLayoutHook = noBorders $ avoidStruts
    $ onWorkspace wsEdit (Full ||| tiled ||| Mirror tiled)
    -- $ onWorkspace wsTerm (Full ||| tiled ||| Mirror tiled)
    $ onWorkspace wsChat (Full ||| tiled ||| Mirror tiled)
    -- $ onWorkspace wsMisc (Circle)
    -- $ onWorkspace wsTest simplestFloat
    $ onWorkspace wsTest (tiled ||| Mirror tiled ||| Full) tiled
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
            -- , className =? "skype"  --> doShift wsChat
            ]

myLogHook xmproc = dynamicLogWithPP $ xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }

-- A list of custom keys
clearConflictKeys :: XConfig a -> XConfig a
clearConflictKeys cf@(XConfig {modMask = myModMask}) =
  removeKeys cf [ (myModMask, xK_n)
                , (myModMask, xK_m)
                , (myModMask, xK_j)
                , (myModMask, xK_k)
                , (myModMask, xK_r)
                ]

myKeys :: XConfig Layout -> Map (ButtonMask, KeySym) (X ())
myKeys (XConfig {modMask = myModMask}) = M.fromList
    [
      ((myModMask, xK_F1), spawn "chromium-browser")
    , ((myModMask, xK_F2), spawn "emacsclient -c")
    , ((myModMask, xK_F3), spawn "google-chrome")
    , ((myModMask, xK_F4), spawn "firefox")
    --, ((myModMask, xK_F5), spawn "thunar")
    , ((myModMask, xK_F5), spawn "nautilus --browser")

    , ((myModMask .|. shiftMask, xK_l), spawn "xscreensaver-command --lock")

      -- launcher keys
    , ((myModMask, xK_p), spawn "gmrun")
    , ((myModMask .|. shiftMask, xK_p), spawn "dmenu-with-yeganeshdmenu")

      -- Toggle struts
    , ((myModMask, xK_a), sendMessage ToggleStruts)

      -- Close window
    -- , ((myModMask, xK_c), kill)

      -- focus to master win
      , ((myModMask .|. shiftMask, xK_m), windows W.focusMaster)

        -- Full float
     , ((myModMask .|. shiftMask, xK_f), fullFloatFocused)

      -- Centered rectangle float
     , ((myModMask .|. shiftMask, xK_r), rectFloatFocused)

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

    ]
  where
    -- Function to fullFloat a window
    fullFloatFocused = withFocused $ \f -> windows =<< appEndo `fmap` runQuery
                          doFullFloat f

    -- Function to rectFloat a window
    rectFloatFocused = withFocused $ \f -> windows =<< appEndo `fmap` runQuery
                          (doRectFloat $ RationalRect 0.02 0.05 0.96 0.9) f
