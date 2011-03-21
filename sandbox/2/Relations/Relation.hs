{-# LANGUAGE FlexibleContexts #-}
module Relation where

import Control.Arrow

import Tuple.TupleConcat

import qualified Data.List as List
import Data.Function
import Data.Monoid
import Data.Foldable

import qualified Data.Sequence as Seq
import Data.Sequence(Seq)

import Prelude hiding (foldl, sum, head, elem)
import qualified Prelude

class (Functor r) => Relation r where
    -- relat algebra
    minus 		:: (Ord a) =>
			   r a -> r a -> r a
    union 		:: r a -> r a -> r a
    selection	 	:: (a -> Bool) -> r a -> r a

    -- other
    insert 		:: a -> r a -> r a
    rempty		:: r a
    joinG		:: (a -> b -> Bool) ->
                           (a -> b -> c) ->
                           r a -> r b -> r c
    partition   	:: (Ord k) => (a -> k) -> Partition r k a

    --
projection :: (Relation r) => (a -> b) -> r a -> r b
projection = fmap

sortBy 			=  undefined
flattenR		=  undefined

singleton		:: (Relation r) => a -> r a
singleton		=  (`insert` rempty)

join p ra rb		=  joinG p (,) ra rb

cartProduct ra rb	=  projection (uncurry (|++|)) $ join (const (const True)) ra rb


instance Relation [] where
    rempty		= []
    insert		= (:)
    selection		= filter
    union		= (++)
    minus		= (List.\\)
    partition		= listPartition
    joinG p f ra rb	= [(a `f` b) | a <- ra, b <- rb, p a b]

listPartition toKey = Partition fn toKey
    where fn = map (map snd) . List.groupBy ((==) `on` fst) .
               List.sortBy  (compare `on` fst) . map (toKey &&& id)


data Partition r key a =
    Partition { partFn		:: r a -> r (r a)
              , getToKey	:: a -> key
              }

data Row k a = Row { getKey	:: k
                   , getRow	:: a
                   }
               deriving(Show, Read, Eq, Ord)

mkRow :: k -> a -> Row k a
mkRow = Row

mapRow :: (a -> b) -> Row k a -> Row k b
mapRow fn (Row k a) = Row k (fn a)

mapKey :: (k -> k') -> Row k a -> Row k' a
mapKey fn (Row k a) = Row (fn k) a



projectionK :: (Relation r) => (a -> b) -> r (Row k a) -> r (Row k b)
projectionK fn = projection (mapRow fn)
selectionK :: (Relation r) => (a -> Bool) -> r (Row k a) -> r (Row k a)
selectionK p   = selection (p . getRow)
joinSK :: (Relation r) =>
          (a -> b -> Bool) -> (a -> b -> c) -> r (Row k a) -> r (Row k1 b) -> r (Row (k, k1) c)
joinSK p fn rka rk1b = joinG p' fn' rka rk1b
    where p'  (Row _ a) (Row _  b) = p a b
          fn' (Row k a) (Row k1 b) = Row (k, k1) (fn a b)

sortByK :: (Relation r) => (a -> a -> Ordering) -> r (Row k a) -> r (Row k a)
sortByK cmp = sortBy (cmp `on` getRow)

partitionK :: (Relation r, Ord k1) => (a -> k1) -> Partition r k1 (Row k a)
partitionK fn = partition fn'
    where fn' (Row k a) = fn a
