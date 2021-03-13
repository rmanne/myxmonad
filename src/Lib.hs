module Lib
    ( xmonadConfig
    ) where

import XMonad
import XMonad.Actions.CycleWS(nextWS, prevWS, shiftToNext, shiftToPrev, toggleWS)
import XMonad.Hooks.ManageDocks(docks, avoidStruts, ToggleStruts(ToggleStruts))
import XMonad.Layout.Minimize(minimize, minimizeWindow, MinimizeMsg(RestoreNextMinimizedWin))
import XMonad.Layout.NoBorders(smartBorders)
import XMonad.Layout.Spacing(smartSpacing)
import XMonad.Layout.ThreeColumns(ThreeCol(ThreeCol))
import XMonad.Operations(withFocused)

import qualified XMonad.StackSet as W

import qualified XMonad.Actions.XMobars as XMobars
import qualified XMonad.Util.Capture as Capture
import qualified XMonad.Util.URxvt as URxvt

--import Graphics.X11.ExtraTypes.XF86

import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)
import Data.Maybe(fromJust)

import System.Exit(exitWith, ExitCode(ExitSuccess))

--termHere :: XConfig Layout -> X ()
--termHere conf = focused >>= \window -> maybeMapM (runQuery className) window >>= \case
--  Just "Alacritty" -> spawn $ "sleep 0.1 && xdotool key --clearmodifiers --window " ++ show (fromJust window) ++ " 'super+n'"
--  _ -> spawn (terminal conf)
--  where
--    focused :: X (Maybe Window)
--    focused = withWindowSet (return . W.peek)

    -- this was impossible to search for in the first place, and i have no clue how fmapMaybeM and maybeMapM are different but the signature is the same...
--    maybeMapM :: Monad m => (a -> m b) -> (Maybe a -> m (Maybe b))
--    maybeMapM _ Nothing  = return Nothing
--    maybeMapM m (Just x) = Just <$> m x

xmonadKeys :: XConfig Layout -> Map (KeyMask, KeySym) (X ())
xmonadKeys conf@(XConfig { modMask = modm, borderWidth = bw }) = Map.fromList $
    --[ ((modm .|. shiftMask, xK_Return), URxvt.home)
    --, ((modm              , xK_n     ), URxvt.copy)
    --[ ((modm              , xK_n     ), termHere conf)
    [ ((modm .|. shiftMask, xK_Return), spawn (terminal conf))
    , ((modm              , xK_p     ), spawn "rofi -modi drun -show drun -sorting-method fzf -sort -theme solarized")

    , ((modm .|. shiftMask, xK_c     ), kill)
 
    , ((modm              , xK_space ), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    , ((modm              , xK_Tab   ), toggleWS)

    , ((modm              , xK_j     ), windows W.focusDown)
    , ((modm              , xK_k     ), windows W.focusUp  )
    --, ((modm              , xK_m     ), windows W.focusMaster)
    , ((modm              , xK_m     ), withFocused minimizeWindow)
    , ((modm .|. shiftMask, xK_m     ), sendMessage RestoreNextMinimizedWin)
    , ((modm              , xK_Return), windows W.swapMaster)
 
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    , ((modm              , xK_h     ), prevWS)
    , ((modm              , xK_l     ), nextWS)
    , ((modm .|. shiftMask, xK_h     ), shiftToPrev)
    , ((modm .|. shiftMask, xK_l     ), shiftToNext)

    , ((modm              , xK_t     ), withFocused $ windows . W.sink)

    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    , ((modm              , xK_b     ), sendMessage ToggleStruts)

    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    ]
    ++
    -- TODO: turn workspaces into proper project-based layouts
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    -- custom mappings
    [ ((modm              , xK_s), Capture.screenshotFocus bw)
    , ((modm .|. shiftMask, xK_s), Capture.screenshot)
    , ((modm              , xK_d), Capture.screenshotWorkspace)
    , ((modm .|. shiftMask, xK_d), Capture.screenshotDesktop)

    , ((modm, xK_r), Capture.record)

    , ((modm              , xK_u), XMobars.update)
    , ((modm .|. shiftMask, xK_u), XMobars.reset)

    --, ((modm, xK_0    ), spawn "mpc prev")
    --, ((modm, xK_minus), spawn "mpc toggle")
    --, ((modm, xK_equal), spawn "mpc next")

    --, ((0, xF86XK_AudioMute        ), spawn "pactl set-sink-mute 1 toggle")
    --, ((0, xF86XK_AudioLowerVolume ), spawn "pactl set-sink-volume 1 -5%")
    --, ((0, xF86XK_AudioRaiseVolume ), spawn "pactl set-sink-volume 1 +5%")

    -- lels
    , ((modm, xK_slash),  -- send shrug emoji
        spawn "sleep 0.1 && /home/.bin/shrug")
    -- this is so garbage; how did i fix this solution with maim/slop before?
    -- prolly check it out
    ]

xmonadConfig = docks $ def
    { terminal = "alacritty"
    , focusFollowsMouse = False
    , mouseBindings = \_ -> Map.empty
    , borderWidth = 2
    , normalBorderColor = "#000000"
    , keys = xmonadKeys
    --, manageHook = URxvt.manageHook <+> manageHook def
    , layoutHook = avoidStruts $ minimize $ smartSpacing 1 $ smartBorders $ ThreeCol 1 (3/100) (1/3) ||| Tall 1 (3/100) (1/2) ||| Full
    , logHook = XMobars.logHook <+> logHook def
    --, handleEventHook = URxvt.eventHook <+> handleEventHook def
    }
