{-# LANGUAGE LambdaCase #-}
module XMonad.Util.Capture(
  screenshot, screenshotFocus, screenshotWorkspace, screenshotDesktop,
  record
) where

import XMonad(X, XID, io, spawn, spawnPID,  -- basic X/IO things
              withWindowSet,                -- actions
              ExtensionClass(initialValue), -- for extensible state

              -- the following for handling window limits
              rect_x, rect_y, rect_height, rect_width, scaleRationalRect,
              floatLocation, screenRect)

import XMonad.StackSet(screenDetail, peek, current)

import qualified XMonad.Util.ExtensibleState as State
import XMonad.Util.Run(runProcessWithInput)

import System.Posix.Types(ProcessID)
import System.Posix.Signals(signalProcess, sigINT)

import Data.List(intercalate)

newtype CaptureState = CaptureState (Maybe ProcessID)
instance ExtensionClass CaptureState where
  initialValue = CaptureState Nothing

slop :: X String
slop = runProcessWithInput "slop" ["-f", "(%x,%y,%w,%h)", "-p", "-5", "-b", "5"] ""

withSlop :: ((Word, Word, Word, Word) -> X ()) -> X ()
withSlop f = read <$> slop >>= \case (_, _, 0, 0) -> return (); x -> f x

maim :: (Show a, Show b) => (a, a, b, b) -> X ()
maim (x, y, w, h) = spawn $ "maim ~/$(date +%F-%T).png -g " ++ geometry
  where geometry = show w ++ "x" ++ show h ++ "+" ++ show x ++ "+" ++ show y

record :: X ()
record = State.get >>= \s -> case (\(CaptureState x) -> x) s of
  Just id -> io (signalProcess sigINT id) >> State.put (CaptureState Nothing)
  Nothing -> withSlop $ \d -> spawnPID (intercalate " " ("recordmydesktop" : args d))
                                >>= \id -> State.put (CaptureState (Just id))
  where args (x, y, w, h) = [ "-x", show (if x == 0 then 1 else x)
                            , "-y", show (if y == 0 then 1 else y)
                            , "--width", show w, "--height", show h
                            , "-o", "/home/out", "--no-sound"
                            , "--on-the-fly-encoding", "--no-wm-check" ]

screenshotFocus :: Integral a => a -> X ()
screenshotFocus bw =
  -- TODO: due to a bug in xorg 1.19, the following doesn't work:
  --  Just w' -> spawn $ "maim ~/$(date +%F-%T).png -i " ++ w'
  withWindowSet $ \w -> case peek w of
    Nothing -> withSlop maim
    Just w' -> floatLocation w' >>= \(_, r') ->
      let
        r = scaleRationalRect (screenRect (screenDetail (current w))) r'
      in
        maim (rect_x r + fromIntegral bw, rect_y r + fromIntegral bw,
              rect_width r - fromIntegral bw * 2, rect_height r - fromIntegral bw * 2)

screenshotWorkspace :: X ()
screenshotWorkspace = withWindowSet $ \w ->
  let r = screenRect $ screenDetail $ current w
  in  maim (rect_x r, rect_y r, rect_width r, rect_height r)

screenshotDesktop :: X ()
screenshotDesktop = spawn "maim ~/$(date +%F-%T).png"

screenshot :: X ()
screenshot = withSlop maim
