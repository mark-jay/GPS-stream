module API where

import Data.Time

import RMC.Protobuf.RMC.RMC 	as RMC

-- fromGregorian :: Integer -> Int -> Int -> Day
rmcToDay :: RMC -> Day
rmcToDay = undefined

-- secondsToDiffTime :: Integer -> DiffTime
-- picosecondsToDiffTime :: Integer -> DiffTime
rmcToUTCTime :: RMC -> UTCTime
rmcToUTCTime rmc = UTCTime (rmcToDay rmc) (secondsToDiffTime (undefined rmc))

