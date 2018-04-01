{-# LANGUAGE LambdaCase #-}
module XMonad.Actions.XMobars(logHook, update, reset) where

import Prelude hiding (rem)

import XMonad(X, XID, io, spawn, spawnPID,  -- basic X/IO things
              withWindowSet,                -- actions
              ExtensionClass(initialValue), -- for extensible state
              ScreenId(S), WindowSet)

import XMonad.StackSet(current, visible, screen)

import XMonad.Hooks.DynamicLog(PP(ppCurrent, ppTitle, ppOutput, ppVisible),
                               wrap, shorten,
                               dynamicLogWithPP, xmobarPP, xmobarColor)

import XMonad.Layout.IndependentScreens(countScreens)
import XMonad.Util.Run(spawnPipe)

import System.IO(Handle, hClose, hPutStrLn)

import qualified XMonad.Util.ExtensibleState as State
import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)

newtype XMobarsState = XMobarsState { bars :: Map ScreenId Handle }
instance ExtensionClass XMobarsState where
  initialValue = XMobarsState Map.empty

insert :: ScreenId -> Handle -> XMobarsState -> XMobarsState
insert s h (XMobarsState m) = XMobarsState (Map.insert s h m)

delete :: ScreenId -> XMobarsState -> XMobarsState
delete s (XMobarsState m) = XMobarsState (Map.delete s m)

add :: ScreenId -> X ()
add (S s) = (\h -> State.modify (insert (S s) h)) =<<
  spawnPipe ("/home/.local/bin/xmobar -x " ++ show s ++ " /home/.config/linux/xmobar.hs")
              
rem :: ScreenId -> X ()
rem s = (Map.lookup s . bars) <$> State.get >>= \case
  Nothing -> return ()
  Just h  -> io (hClose h) >> State.modify (delete s)

update' :: XMobarsState -> X ()
update' (XMobarsState m) = withWindowSet $ \w ->
  let
    s = map screen (current w : visible w)
    s' = Map.fromList $ zip s $ repeat ()
    (m', d) = Map.partitionWithKey (\k _ -> Map.member k s') m
    a = Map.filterWithKey (\k _ -> not $ Map.member k m') s'
  in
    Map.foldMapWithKey (\s _ -> rem s) d >> State.put (XMobarsState m') >>
      Map.foldMapWithKey (\s _ -> add s) a

update :: X ()
update = State.get >>= update'

reset :: X ()
reset = (Map.keys . bars) <$> State.get >>= mapM_ (\k -> rem k >> add k)

logHook :: X ()
logHook = withWindowSet $ \w ->
  bars <$> State.get >>= Map.foldMapWithKey
    (\s h -> dynamicLogWithPP xmobarPP
              { ppOutput  = hPutStrLn h
              , ppTitle   = xmobarColor "green" "" . shorten 70
              , ppCurrent = xmobarColor "yellow" "" .
                              if s == screen (current w)
                                then wrap "[" "]"
                                else wrap "(" ")"
              , ppVisible = xmobarColor "cyan" "" .
                              if s == screen (current w)
                                then wrap "(" ")"
                                else wrap "<" ">"
              })
