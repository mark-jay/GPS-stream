{-# LANGUAGE GADTs, PatternGuards #-}
module Syntax.Window where

import RelMap
import Relation
import Group
import Data.Monoid

import Data.Map(Map)
import qualified Data.Map as Map

import qualified Data.List as List

{-
data Window k inp out where
    Window  :: RelMap inp out -> Window k inp out
    WindowP :: (Ord k) => RelMap inp out -> Partition k inp -> Window k inp out

wRelMap (Window  rm)   = rm
wRelMap (WindowP rm _) = rm
-}

--------------------------------------------------------------------------------
-- Aggrs
data Aggr inp outp gr = Aggr {
      aToGroup   :: outp -> gr
    , aFromGroup :: gr -> outp
    , aInToOut	 :: inp -> outp
    }

mkAggr :: (Group g, Eq outp) => (outp -> g) -> (g -> outp) ->
          (inp -> outp) -> Aggr inp outp g
mkAggr toGroup fromGroup = Aggr toGroup fromGroup

{-
sum :: (Num a) => (inp -> a) -> Aggr inp a (Sum a)
sum =  mkAggr Sum getSum
-}



data PartitionS r key inp = 
    PartitionS { partRelation	:: r [inp]
               , getToKey	:: inp -> key
               }

partition :: (Relation r, Ord key) => (r inp, inp -> key) -> PartitionS r key inp
partition = undefined

sum :: (Group g) => (inp -> outp) -> Aggr inp outp g
sum = undefined

-- sum :: (Group g) => (inp -> outp) -> Aggr inp outp g
-- partition :: (Relation r, Ord key) => (r inp, inp -> key) -> Partition r inp key
overP :: (Group g, Ord key) => Aggr inp outp g -> PartitionS r key inp -> (inp -> outp)
aggr `overP` part = f
    where
      f inp | Just outp <- g inp  = outp
            | otherwise		  = undefined
            where g = (`Map.lookup` hashMap) . toKey
      (Aggr toGroup fromGroup inToOut) = aggr
      (PartitionS partRel toKey) = part
      hashMap = Map.insert undefined undefined Map.empty

-- projection (foldl' mappend mempty .  map (aToGroup . aInToOut))

-- select (sum(fieldFun) `overP` partition(table, gbFun), ...)

--------------------------------------------------------------------------------
-- partition

-- select (sum(fieldFun) `overP` partition(table, gbFun), ...)

{-
overP :: (Group g) => Aggr inp outp g -> Partition key a ->
fn `overP` part = undefined
-}


{-
overW :: RelMap a b -> Window a b b -> RelMap a b
overW = undefined
-}

--------------------------------------------------------------------------------
-- overs

{-
overW :: RelMap a b -> Window a b b -> RelMap a b
overW = undefined

overP :: r a -> Partition key a -> r a
overP = undefined

overWP :: RelMap a b -> Window a b -> RelMap a b
overWP = undefined
-}
fst' :: (Int, Int) -> Int
fst' = fst

snd' :: (Int, Int) -> Int
snd' = snd
