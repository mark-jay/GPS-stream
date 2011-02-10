{-# LANGUAGE FlexibleContexts #-}
module Relation where

import Data.Monoid
import Control.Arrow

import TupleUnion

import qualified Data.List as List
import Data.Function

import Prelude hiding (foldl, sum, head)
import qualified Prelude

class Relation r where
    insert 		:: a -> r a -> r a
    rempty		:: r a
    foldl		:: (b -> a -> b) -> b -> r a -> b
    partition   	:: (Ord k, Relation r) => (a -> k) -> Partition r k a

    -- relat algebra
    minus 		:: (Eq a) => 
			   r a -> r a -> r a
    minus ra1 ra2	=  foldl fn rempty ra1
        where fn acc b | b `member` ra2 = acc
                        | otherwise	= b `insert` acc
    union 		:: r a -> r a -> r a
    union ra		=  foldl (flip insert) ra
    projection	 	:: (a -> b) -> r a -> r b
    projection	fn 	=  foldl (\ b a -> fn a `insert` b) rempty
    selection	 	:: (a -> Bool) -> r a -> r a
    selection	p 	=  foldl p' rempty
        where p' b a | p a 	 = a `insert` b
                     | otherwise = b

    -- other
    singleton		:: a -> r a
    singleton		=  (`insert` rempty)
    over		:: (Eq k) =>
                           (r a -> b) ->
                           Partition r k a ->
                           (r a -> r (k, b))
    (agr `over` Partition fn toKey) ra 
        = let rra = fn ra
              rkb = projection (toKey . head &&& agr) rra
              pred a (k,b) 
                 | toKey a == k = True
                 | otherwise  	= False
          in projection (toKey *** snd) $ join pred ra rkb

    head		:: r a -> a
    head		=  Prelude.head . toList

    toList		:: r a -> [a]
    toList		=  foldl (flip (:)) []

    fromList		:: [a] -> r a
    fromList		=  Prelude.foldl (flip insert) rempty

    member 		:: (Eq a) => a -> r a -> Bool
    member a		=  projection (==a) >>> foldl (||) False

    joinS		:: (a -> b -> Bool) ->
                           (a -> b -> c) ->
                           r a -> r b -> r c
    joinS p pc ra rb 	=  foldl fn  rempty ra
        where fn accA elA = foldl fn' rempty rb `union` accA
                 where fn' accB elB | p elA elB  = (pc elA elB) `insert` accB
                                    | otherwise  = accB

    join 		:: (a -> b -> Bool) ->
                           r a -> r b -> r (a,b)
    join p ra rb	=  joinS p (,) ra rb

    cartProduct 	:: (TupleUnion a b c) => 
			   r a -> r b -> r c
    cartProduct ra rb	=  projection (uncurry (|+|)) $ join (const (const True)) ra rb

    sortBy 		:: (a -> a -> Ordering) -> r a -> r a
    sortBy pred ra	=  let h = head ra
                           in sortBy pred (selection ((==LT) . (`pred` h)) ra) `union`
                                  (selection  ((==EQ) . (`pred` h)) ra)  `union`
                                  sortBy pred (selection ((==GT) . (`pred` h)) ra)

    flattenR		:: r (r a) -> r a
    flattenR		=  foldl fn rempty
        where fn acc el = el `union` acc

instance Relation [] where
    foldl		= Prelude.foldl
    rempty		= []
    singleton		= return
    insert		= (:)
    partition		= listPartition

listPartition toKey = Partition fn toKey
    where fn = map (toKey &&& id) >>>
               List.sortBy  (compare `on` fst) >>>
               List.groupBy ((==)    `on` fst) >>>
               map (map snd)

--------------------------------------------------------------------------------------------------------

data Partition r key a = 
    Partition { partFn	:: r a -> r (r a)
              , toKey	:: a -> key
              }

--------------------------------------------------------------------------------------------------------

newtype WrappedRelation r a = WR { getRelation :: r a }
    deriving(Show, Read)

instance (Relation r) => Functor (WrappedRelation r) where
    fmap fn (WR ra) = WR (projection fn ra)

instance (Relation r) => Monoid (WrappedRelation r a) where
    WR ra `mappend` WR rb = WR (ra `union` rb)
    mempty		  = WR rempty

instance (Relation r) => Monad (WrappedRelation r) where
    return a   		= WR (singleton a)
    ma >>= pmb	 	= WR $ foldl union rempty $ getRelation $ fmap (getRelation . pmb) ma

--------------------------------------------------------------------------------------------------------

{-
test :: [(Integer, Int)] -> [((Integer, Int), Integer)]
test = sum(fst) `over` partition(snd)
-- test [(1,1),(2,1),(3,2),(4,2),(10,10)]

test1 :: (TupleUnion (Char, Char) b c) => b -> c
test1 = (('a','b') |+|)
-- test1 <$> (pure (\(a, b, c, d)->(a,b)) <*> cartProduct [(1,2),(3,4)] [(5,'a')])
-}