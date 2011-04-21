{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
module Timable( mkTimable
              , getData
              , TimableData
              , Timable(..)
              ) where

import Data.Time.Clock
import Data.Time
import Data.Tuple.All

import Tools.TemplateTools(timableInstances)

class Timable t where
    toTime :: t -> UTCTime

$(timableInstances 10)

-- TimableData
newtype TimableData a = TimableData (UTCTime, a)

mkTimable :: UTCTime -> a -> TimableData a
mkTimable time a = TimableData (time, a)

getData :: TimableData a -> a
getData (TimableData (_, data')) = data'

instance Timable (TimableData a) where
    toTime (TimableData (time, _)) = time