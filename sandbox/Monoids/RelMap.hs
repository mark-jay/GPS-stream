{-# LANGUAGE PatternGuards, MultiParamTypeClasses, FunctionalDependencies,
  FlexibleContexts, FlexibleInstances #-}
module RelMap
    where

import Data.Foldable
import Data.Tuple.All
import Data.Time		as Time
import Data.Time.Clock		as Time
import Data.Monoid
import Data.Function(on)
import qualified Data.List  as List
import Data.Maybe as Maybe
import Data.ByteString.Internal as BS
import qualified Data.Sequence as Seq
import Data.Sequence(Seq, (<|), (|>), ViewL(..), ViewR(..))
import qualified Data.Set as Set
import Data.Set(Set)

import System.Random

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad
import Control.Monad.Identity

import Prelude hiding((.), id, foldl, foldr)

import Queue
import Relation
import Aggrs
import Utils
import Queue
import Tuple.TupleApply

--------------------------------------------------------------------------------
-----------------------------   data structure and api   -----------------------
--------------------------------------------------------------------------------

data RelMapGen f inp outp = RM
    { rmMapper 	:: inp -> RelMapGen f inp outp
    , rmAcc	:: f outp
    }

type RelMap = RelMapGen Seq


fromFnMb 	  :: (Queue f) => (a -> Maybe b) -> RelMapGen f a b
fromFnMb fn =  RM (mkMapper qempty) qempty
    where mkMapper s inp
              | Just outp <- fn inp 	= let s' = (s `qpush` outp)
                                          in s' `seq` RM (mkMapper s') s'
              | otherwise		= RM (mkMapper s) s

fromFn :: (Queue f) => (a -> b) -> RelMapGen f a b
fromFn = fromFnMb . (Just .)

-- produces unfeedable RelMaps
fromQueue :: (Queue f) => f outp -> RelMapGen f inp outp
fromQueue q = rm
    where rm = RM (const rm) q

emptyRM :: (Queue f) => RelMapGen f inp outp
emptyRM = fromQueue qempty

feedElms :: (Foldable t) => RelMapGen f inp out -> t inp -> RelMapGen f inp out
feedElms = foldl (\ rm el -> rmMapper rm el)

foldyRM :: (a -> b -> b) -> b -> RelMap a b
foldyRM fn zero = RM (mkMapper seqZero) seqZero
    where mkMapper acc el = acc' `seq` RM (mkMapper acc') acc'
              where acc' = Seq.adjust (fn el $!) 0 acc -- FIXME just
          seqZero = Seq.singleton zero

lastRow :: (a -> b) -> RelMap a b
lastRow f = RM f' Seq.empty
    where f' a = RM f' $ Seq.singleton $ f a

flattenRM :: (Foldable f) => RelMap a (f b) -> RelMap a b
flattenRM (RM f oldAcc) = RM (mkMapper oldAcc) (toSeq oldAcc)
    where
      mkMapper acc el = RM (mkMapper mappedAcc) acc'
          where mappedAcc = rmAcc $ f el
                acc' = toSeq mappedAcc

      toSeq :: (Foldable f) => Seq (f a) -> Seq a
      toSeq = foldl fn Seq.empty
          where fn acc el = foldl (|>) acc el

-- more general type than (.)
rmComp :: (Foldable f) => RelMapGen g b c -> RelMapGen f a b -> RelMapGen g a c
rm2@(RM fn2 acc2) `rmComp` rm1@(RM fn1 acc1) = RM fn3 acc3
    where fn3 inp 	= rm2 `rmComp` fn1 inp
          acc3	 	= rmAcc $ feedElms rm2 acc1

-- merges 2 stream
merge :: (Foldable f, Queue f) => RelMapGen f a b -> RelMapGen f a b -> RelMapGen f a b
rm1@(RM fn1 acc1) `merge` rm2@(RM fn2 acc2) = RM fn3 acc3
    where fn3 inp 	= fn1 inp `merge` fn2 inp
          acc3		= flattenQ $ qpush (qpush qempty acc1) acc2

--------------------------------------------------------------------------------
------------------------------   instances  ------------------------------------
--------------------------------------------------------------------------------
instance (Show outp, Show (f outp)) => Show (RelMapGen f inp outp) where
    showsPrec _ (RM _ seq) = ("RM _ " ++) . (show seq ++)

instance (Queue f, Foldable f) => Category (RelMapGen f) where
    id = RM (mkMapper qempty) qempty
        where mkMapper acc inp = RM (mkMapper acc) (acc `qpush` inp)
    (.) = rmComp

instance Relation (RelMapGen Seq inp) where
    -- FIXME (RelMapGen Seq inp) must be (RelMapGen f inp)
    (RM fn1 acc1) `union` rm2@(RM fn2 acc2) = RM fn3 acc3
        where acc3   = acc1 `qunion` acc2
              fn3 el = fn1 el `union` fn2 el

    selection p ra   = fromFnMb (toMaybeFn p) . ra
        where toMaybeFn :: (a -> Bool) -> a -> Maybe a
              toMaybeFn p a | p a 	  = Just a
                            | otherwise = Nothing

    RM fn1 acc1 `minus` RM fn2 acc2 = RM fn3 acc3
        where acc3   = acc1 `seqDiff` acc2
              fn3 el = fn1 el `minus` fn2 el

    -- other
    a `insert` RM fn acc =  RM (\el -> a `insert` fn el) (acc `qpush` a)

    rempty		 =  emptyRM

    joinG p s (RM fn1 acc1) (RM fn2 acc2) = RM fn3 acc3
        where acc3    = joinGSeq p s acc1 acc2
              fn3 inp = joinG p s (fn1 inp) (fn2 inp)
--    partition   	 =  undefined

joinGSeq :: (Foldable f, Queue f) =>
            (a -> b -> Bool) -> (a -> b -> c)
         -> f a -> f b -> f c
joinGSeq p s s1 s2 = Queue.flattenQ $ foldl (\ acc el -> acc `qpush` fn el) qempty s1
    where
      fn el1 = foldl fn' qempty s2
          where
            fn' acc el2 | p el1 el2  = acc `qpush` s el1 el2

instance (Queue f, Foldable f) => Functor (RelMapGen f inp) where
    fmap fn ra = fromFn fn . ra

instance (Foldable f, Queue f) => Applicative (RelMapGen f inp) where
    pure  = fromQueue . qsingleton

    RM fn1 acc1 <*> rm2@(RM fn2 acc2) = RM fn3 acc3
        where acc3   = joinGSeq (\_ _-> True) ($) acc1 acc2
              fn3 el = fn1 el <*> fn2 el


-- let a = fromQueue $ Seq.fromList [1, 2, 3] :: RelMap Int Int
-- let f = \a -> fromQueue $ Seq.fromList [1, a, 3] :: Int -> RelMap Int Int
instance (Queue f, Foldable f, Functor f) => Monad (RelMapGen f inp) where
    return = pure

    -- ignores produced functions
    -- RelMap inp (RelMap inp outp) -> RelMap inp outp?
    RM _ acc1 >>= prm2 = foldl merge emptyRM . fmap prm2 $ acc1

{-
------------------------------------
------ some bad ideas

instance Queue (RelMapGen Seq inp) where
    qempty = emptyRM
    qpush  = flip insert
    qpop   = undefined

instance (Read outp) => Read (RelMap inp outp) where
    readsPrec _ str | "RM _ " `List.isPrefixOf` str =
                        let cuttedStr = drop 5 str
                        in [(first fromSeq r) | r <- reads cuttedStr]
                    | otherwise			    = []
-}
