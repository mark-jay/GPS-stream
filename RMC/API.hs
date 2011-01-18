module RMC.API(calcRMCSum, rmcToDay, 
               rmcToDiffTime, rmcToUTCTime,
               module RMC)
where

import RMC.Protobuf.RMC.RMC		as RMC

import Data.Time
import Control.Monad
import Data.Bits(xor)
import Data.Char(ord)
import Data.ByteString.Internal(c2w, w2c)
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString       as BS


calcRMCSum :: BS.ByteString -> Int
calcRMCSum input = xorBits (f input)
    where xorBits = foldl xor 0 . map ord . BSC8.unpack
          -- select part we need
          f = BS.takeWhile (/= c2w '*') . BS.tail . BS.dropWhile (/= c2w '$')

--foldl xor 0 $ map ord $ BSC8.unpack $ f $ BSC8.pack "$GPRMC,125504.049,,5542.2389,N,03741.6063,E,0.06,25.82,200906,,E*3B\13"

----------------------------------------
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
----------------------------------------

