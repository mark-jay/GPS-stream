module RMC.API where

import Data.Time
import Control.Monad

import RMC.Protobuf.RMC.RMC 	as RMC

rmcToDay :: RMC -> Maybe Day
rmcToDay = liftM (f . fromIntegral) . date
    where f :: Int -> Day
          f num = fromGregorian (fromIntegral years) (fromIntegral months) (fromIntegral days)
              where years  = num `div` (12*31)
                    months = (num - years*12*31) `div` 31
                    days   = (num - years*12*31) - months*31

rmcToDiffTime :: RMC -> Maybe DiffTime
rmcToDiffTime = liftM mkTime . time
    where mkTime :: Double -> DiffTime
          mkTime = picosecondsToDiffTime . truncate . (* 10^12)

rmcToUTCTime :: RMC -> Maybe UTCTime
rmcToUTCTime rmc = mkUTCTime (rmcToDay rmc) (rmcToDiffTime rmc)
    where mkUTCTime day time = case (day, time) of
                                 (Just d, Just t)  -> Just $ UTCTime d t
                                 _		   -> Nothing
