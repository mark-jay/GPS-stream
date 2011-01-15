module Utils where

import System.Exit
import System.IO
import IO

import Data.Maybe(listToMaybe)
import Data.Typeable
import Data.Time.Clock

import qualified Text.ProtocolBuffers.Basic
import qualified Data.ByteString.Internal 
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LazyBS

import System.ZMQ as ZMQ

import Control.Monad
----------------------------------------------------------------------
exitWithErr errMessage = do hPutStrLn stderr errMessage
                            exitFailure

docAndExit msg = do putStrLn msg
                    exitSuccess

----------------------------------------------------------------------

maybeRead :: (Read a) => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads 

askFor :: String -> IO String
askFor string = do putStrLn string
                   r <- getLine
                   return r

----------------------- cast utils ----------------------------
-- FIXME does not work
fromLazyBS :: LazyBS.ByteString
           -> BS.ByteString
fromLazyBS = BS.concat . LazyBS.toChunks

toLazyBS :: BS.ByteString
          -> LazyBS.ByteString
toLazyBS = LazyBS.fromChunks . return

----------------------- debug utils ----------------------------

getTime :: (a -> b) -> a -> IO (b, NominalDiffTime)
getTime f x = do beg <- getCurrentTime
                 let r = f x
                 end <- r `seq` getCurrentTime
                 return(r, diffUTCTime end beg)

getTimeIO :: (a -> IO b) -> a -> IO (b, NominalDiffTime)
getTimeIO f x = do beg <- getCurrentTime
                   r <- f x
                   end <- r `seq` getCurrentTime
                   return(r, diffUTCTime end beg)

--------------------------------------------------------------------------------
-- ZMQ



mkSockToConn :: (SType st) => Context -> st -> [String] -> IO (Socket st)
mkSockToConn context st connectTo = do
  sock <- socket context st
  forM_ connectTo $ \ addr -> do
      connect sock addr
  return sock

mkSockToBind :: (SType st) => Context -> st -> [String] -> IO (Socket st)
mkSockToBind context st bindTo = do
  sock <- socket context st
  forM_ bindTo $ \ addr -> do
      bind sock addr
  return sock

{-
withContext :: (Context -> IO b) -> IO b
withContext fn = bracket (ZMQ.init 1)
                         (ZMQ.term)
                         (fn)
-}
