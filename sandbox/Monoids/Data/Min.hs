{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Data.Min
-- Copyright   :  (c) Conal Elliott 2008
-- License     :  GNU AGPLv3 (see COPYING)
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Min monoid
----------------------------------------------------------------------

module Data.Min (Min(..)) where


import Data.Monoid (Monoid(..))

import Test.QuickCheck (Arbitrary, CoArbitrary)
-- import Test.QuickCheck.Checkers (EqProp)


-- | Ordered monoid under 'min'.
newtype Min a = Min { getMin :: a }
	deriving (Eq, Ord, Bounded, Read, Show, Arbitrary, CoArbitrary)

instance (Ord a, Bounded a) => Monoid (Min a) where
	mempty = Min minBound
	Min a `mappend` Min b = Min (a `min` b)
