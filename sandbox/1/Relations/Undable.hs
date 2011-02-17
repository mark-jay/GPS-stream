module Undable (Undable(..),
                Avrg(),  mkAvrg,  getAvrg,
                Count(), mkCount, getCount)
    where

import Data.Monoid
import Data.Ratio

class Monoid a => Undable a where
    -- reversed mappend
    rmappend :: a -> a -> a

{-# RULES
"rmappend/undo"   forall a b     . 	a `mappend` b `rmappend` b = a
"rmappend/negate" forall a b     .	(a `mappend` b) `mappend` (mempty `rmappend` b) = a
"rmappend/3"      forall a b c   .	(a `mappend` b `mappend` c) `rmappend` c `rmappend` b = a
"rmappend/4"      forall a b c   .	(a `mappend` b `mappend` c) `rmappend` b `rmappend` c = a
  #-}

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

--------------------------------------------------------------------------------
--------------------------   instances      ------------------------------------
--------------------------------------------------------------------------------
-- monoids
instance (Fractional a) => Monoid (Avrg a) where
    mempty = Avrg 0 0
    Avrg s1 c1 `mappend` Avrg s2 c2 = Avrg (s1 + s2) (c1 + c2)

instance Monoid (Count a) where
    mempty = Count 0
    Count c1 `mappend` Count c2 = Count $ c1 + c2


-- Undables
instance (Num a) => Undable (Sum a) where
    Sum a `rmappend` Sum b = Sum (a - b)

instance (Fractional a) => Undable (Product a) where
    Product a `rmappend` Product b = Product (a / b)

instance (Fractional a) => Undable (Avrg a) where
    Avrg s1 c1 `rmappend` Avrg s2 c2 = Avrg (s1 - s2) (c1 - c2)

instance Undable (Count a) where
    Count c1 `rmappend` Count c2 = Count $ c1 - c2
--------------------------------------------------------------------------------
