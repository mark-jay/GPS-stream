module Utils where

import Data.Time
import Data.Time.Clock
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Foldable
import qualified Data.Sequence as Seq
import Data.Sequence(Seq, (<|), (|>), ViewL(..), ViewR(..), (><))

import Prelude hiding(foldl, foldr)
--------------------------------------------------------------------------------
instance Bounded UTCTime where
    maxBound = read "9999-12-31 23:59:59.9999999999 UTC"
    minBound = read "0000-01-01 00:00:00 UTC"

instance Bounded NominalDiffTime where
    maxBound = maxBound `diffUTCTime` minBound
    minBound = minBound `diffUTCTime` maxBound

instance (Bounded a) => Bounded (Maybe a) where
    maxBound = Just maxBound
    minBound = Just minBound

--------------------------------------------------------------------------------
----- utils

-- FIXME write me better
seqDiff :: (Ord a) => Seq a -> Seq a -> Seq a
seqDiff s1 s2 = Seq.filter p s1
    where p a = Set.member a set
          set = Set.fromList . toList $ s2

flattenS :: Seq (Seq a) -> Seq a
flattenS = foldr (><) Seq.empty
