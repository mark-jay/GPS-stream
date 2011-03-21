{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}
module Aggrs( Avrg(),  mkAvrg,  getAvrg
            , Count(), mkCount, getCount
            )
    where

import Data.Monoid
import Data.Max
import Data.Min
import Data.Ratio

import qualified Data.FingerTree as FingerTree
import Data.FingerTree(FingerTree, Measured)


data Avrg a = Avrg a Int
              deriving(Show, Ord, Eq)

mkAvrg :: (Real a) => a -> Avrg a
mkAvrg  = flip Avrg 1

getAvrg :: (Real a) => Avrg a -> Double
getAvrg (Avrg a b) = f a / fromIntegral b
    where f = fromRational . toRational

newtype Count a = Count { getCount :: Int }
    deriving(Show, Ord, Eq)

mkCount :: b -> Count a
mkCount = const (Count 1)

-- monoid instances
instance (Fractional a) => Monoid (Avrg a) where
    mempty = Avrg 0 0
    Avrg s1 c1 `mappend` Avrg s2 c2 = Avrg (s1 + s2) (c1 + c2)

instance Monoid (Count a) where
    mempty = Count 0
    Count c1 `mappend` Count c2 = Count $ c1 + c2