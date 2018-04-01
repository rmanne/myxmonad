{-# LANGUAGE ScopedTypeVariables #-}
module XMonad.Util.URxvt(manageHook, eventHook, copy, home) where

import Prelude hiding (last, rem)
--import XMonad hiding (get, manageHook, eventHook)
import XMonad(X, XID, io, withFocused, liftX, -- basic X/IO things
              ExtensionClass(initialValue),   -- for extensible state
              ManageHook, className, ask, (=?), (-->), doF,   -- for manageHook
              Event(DestroyWindowEvent), ev_window, ev_event) -- for eventHook

import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString as ByteString8
import Network.Socket.ByteString(send, sendMany, recv)
import Network.Socket(socket, connect, close,
                      Family(AF_UNIX), SockAddr(SockAddrUnix), SocketType(Stream))
import System.Environment(getEnvironment)
import System.Posix.Files(readSymbolicLink)

import Data.Monoid(All(All))
import Data.Word(Word8)
import Data.Bits(shiftR, shiftL)

import qualified XMonad.Util.ExtensibleState as State
import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)

import Control.Exception(catch)

manageHook :: ManageHook
manageHook = className =? "URxvt" --> ask >>= liftX . add >> doF id

pack :: String -> ByteString.ByteString
pack s =
  let
    b = ByteString.pack s
    z = ByteString.length b
  in
    ByteString.append (ByteString8.pack [to8 (shiftR z 8), to8 z]) b
  where to8 x = fromIntegral x :: Word8

urxvtc :: String -> IO Int
urxvtc d = do
  -- make a socket and connect to my daemon's path
  s <- socket AF_UNIX Stream 0
  connect s $ SockAddrUnix "/home/.cache/urxvtd"

  -- send new client creation command
  _ <- send s $ pack "NEW"

  -- first, send the environment.
  -- NOTE: the environment we use is the one we got when executing xmonad
  getEnvironment >>= sendMany s . map pack . concatMap (\(k, v) -> ["ENV", k ++ "=" ++ v])

  -- then, send the arguments to define the new client
  sendMany s $ map pack ["ARG", "urxvtc", "ARG", "-cd", "ARG", d, "ARG",
                         "-pt", "ARG", "Root", "END"]

  -- finally, recieve the pid of the child and return it
  r <- recv s 3
  close s
  let f i = fromIntegral (ByteString8.index r i) :: Int
  return (shiftL (f 0) 8 + f 1)

data URxvtState = URxvtState { pids :: Map XID Int, last :: Int }
instance ExtensionClass URxvtState where
    initialValue = URxvtState Map.empty 0

add :: XID -> X ()
add k = State.modify $ \d -> d { pids = Map.insert k (last d) $ pids d }

rem :: XID -> X()
rem k = State.modify $ \d -> d { pids = Map.delete k $ pids d }

get :: XID -> X (Maybe Int)
get k = (Map.lookup k . pids) <$> State.get

path :: String -> X ()
path s = io (urxvtc s) >>= \l -> State.modify $ \d -> d { last = l }

-- | start a new terminal in the home directory
home :: X ()
home = path "/home"

here :: Maybe Int -> X ()
here Nothing = path "/home"
here (Just pid) = getPath pid >>= path
  where getPath p = io $ catch getPath' $ \(_ :: IOError) -> return "/home"
          where getPath' = readSymbolicLink ("/proc/" ++ show p ++ "/cwd")

-- | start a new terminal in the same directory as the focused one
-- if a window is not currently being focused, or if the window has no
-- pid, use the default path
copy :: X ()
copy = withFocused (\w -> get w >>= here)

-- | if a window would be destroyed, remove its entry from the map
eventHook :: Event -> X All
eventHook e@(DestroyWindowEvent {}) | ev_window e == ev_event e =
  rem (ev_window e) >> return (All True)
eventHook _ = return $ All True
