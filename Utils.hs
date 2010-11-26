module Utils where

import Data.Maybe(listToMaybe)
import Data.Time.Clock

maybeRead :: (Read a) => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads 


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
