module Utils where

import System.Exit
import System.IO

import Data.Maybe(listToMaybe)
import Data.Typeable
import Data.Time.Clock

import qualified Text.ProtocolBuffers.Basic
import qualified Data.ByteString.Internal 
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LazyBS

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

{-
data LazyBS.ByteString
  = Data.ByteString.Lazy.Internal.Empty
  | Data.ByteString.Lazy.Internal.Chunk !Data.ByteString.ByteString
                                        LazyBS.ByteString
  	-- Defined in Data.ByteString.Lazy.Internal

data LazyBS.ByteString
  = ...
  | Data.ByteString.Lazy.Internal.Chunk !Data.ByteString.ByteString
                                        LazyBS.ByteString
  	-- Defined in Data.ByteString.Lazy.Internal
-}
