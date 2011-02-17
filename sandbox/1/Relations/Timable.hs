{-# LANGUAGE FlexibleInstances #-}
module Timable where

import Data.Time.Clock
import Data.Time
import Data.Tuple.All

class Timable t where
    toTime :: t -> UTCTime

instance Timable UTCTime where
    toTime = id

-- generated code 'Tools.TuplesGen.Main' was called with args 'time 14'
instance Timable (UTCTime, a1) where
    toTime = sel1
instance Timable (UTCTime, a1, a2) where
    toTime = sel1
instance Timable (UTCTime, a1, a2, a3) where
    toTime = sel1
instance Timable (UTCTime, a1, a2, a3, a4) where
    toTime = sel1
instance Timable (UTCTime, a1, a2, a3, a4, a5) where
    toTime = sel1
instance Timable (UTCTime, a1, a2, a3, a4, a5, a6) where
    toTime = sel1
instance Timable (UTCTime, a1, a2, a3, a4, a5, a6, a7) where
    toTime = sel1
instance Timable (UTCTime, a1, a2, a3, a4, a5, a6, a7, a8) where
    toTime = sel1
instance Timable (UTCTime, a1, a2, a3, a4, a5, a6, a7, a8, a9) where
    toTime = sel1
instance Timable (UTCTime, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) where
    toTime = sel1
instance Timable (UTCTime, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) where
    toTime = sel1
instance Timable (UTCTime, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) where
    toTime = sel1
instance Timable (UTCTime, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) where
    toTime = sel1
instance Timable (UTCTime, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) where
    toTime = sel1