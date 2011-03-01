{-# LANGUAGE FlexibleContexts #-}
module Relation where

import Control.Arrow

import Tuple.TupleConcat

import qualified Data.List as List
import Data.Function
import Data.Monoid
import Data.Foldable

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

------------------------------------------------------------
instance Relation [] where
    selection		= filter
    union		= (++)
    minus		= (List.\\)
    partition		= listPartition
    rempty		= []
    insert		= (:)
    joinG p f ra rb	= [(a `f` b) | a <- ra, b <- rb, p a b]

listPartition toKey = Partition fn toKey
    where fn = map (map snd) . List.groupBy ((==) `on` fst) .
               List.sortBy  (compare `on` fst) . map (toKey &&& id)

--------------------------------------------------------------------------------------------------------

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

--------------------
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

{-
overK :: (Table r, Eq k) =>
         (r (Row k1 a) -> r (Row k1 b)) -> Partition r k (Row k1 a) -> r (Row k1 a) -> r (Row k1 b)
(agr `overK` part) rk1a = joinG jPred jUnion rkb rk1a
    where (fn, toKey)	= (partFn part, getToKey part)
          -- :: r (r (Row k1 a))
          rrk1a		= fn rk1a
          -- :: r(k, r(Row k2 b))
          rkrk1b	= projection (toKey . head &&& agr) rrk1a
          -- :: r(Row k b))
          rkb		= flattenR $ projection fn rkrk1b
            where fn (k, rk2b) = projection (mapKey (const k)) rk2b
          jPred (Row k b) rk1a		= k == toKey rk1a
          jUnion (Row k b) (Row k1 a)  	= Row k1 b
-}

--------------------------------------------------------------------------------------------------------

{-
newtype WrappedTable r a = WT { getTable :: r a }
    deriving(Show, Read)

instance (Table r) => Relation (WrappedTable r) where
    minus (WT ra1) (WT ra2)
        = WT $ foldl fn rempty ra1
          where fn acc b | b `elem` ra2 = acc
                         | otherwise = b `insert` acc
    union (WT ra) = WT . foldl (flip insert) ra . getTable
    projection fn = WT . foldl (\ b a -> fn a `insert` b) rempty . getTable
    selection p = WT . foldl p' rempty . getTable
        where p' b a | p a = a `insert` b
                     | otherwise = b

instance (Table r) => Functor (WrappedTable r) where
    fmap fn (WT ra) = WT (projection fn ra)

instance (Table r) => Monoid (WrappedTable r a) where
    WT ra `mappend` WT rb = WT (ra `union` rb)
    mempty		  = WT rempty

instance (Table r) => Monad (WrappedTable r) where
    return a   		= WT (singleton a)
    ma >>= pmb	 	= WT $ foldl union rempty $ getTable $ fmap (getTable . pmb) ma
-}

--------------------------------------------------------------------------------------------------------

{-
test :: [(Integer, Int)] -> [((Integer, Int), Integer)]
test = sum(fst) `over` partition(snd)
-- test [(1,1),(2,1),(3,2),(4,2),(10,10)]

test1 :: (TupleUnion (Char, Char) b c) => b -> c
test1 = (('a','b') |++|)
-- test1 <$> (pure (\(a, b, c, d)->(a,b)) <*> cartProduct [(1,2),(3,4)] [(5,'a')])
-}