{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
module Timable where

import Data.Time.Clock
import Data.Time
import Data.Tuple.All

import Tools.TemplateTools(timableInstances)

class Timable t where
    toTime :: t -> UTCTime

$(timableInstances 10)
