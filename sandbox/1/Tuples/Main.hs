{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies #-}

module Main where

import TupleUnion
import TupleApply

import Control.Applicative
import Control.Arrow
import Control.Monad hiding (join)

import qualified Data.List as List

import Data.Monoid
import Data.Function
import qualified Data.Maybe as Maybe

import Prelude hiding (foldl, sum, head)
import qualified Prelude

class Relation r where
    insert 		:: a -> r a -> r a
    singleton		:: a -> r a
    rempty		:: r a
    foldl		:: (b -> a -> b) -> b -> r a -> b
    partition   	:: (Ord k, Relation r) => (a -> k) -> r a -> (a->k, r (r a))

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
    over		:: (Eq k) =>
                           (r a -> b) ->
                           (r a -> (a->k, r (r a))) ->
                           (r a -> r (a, b))
    (agr `over` part) ra = let (toKey, rra) = part ra
                               rkb	    = projection (toKey . head &&& agr) rra
                               pred a (k,b) 
                                   | toKey a == k = True
                                   | otherwise	  = False
                           in projection (id***snd) $ join pred ra rkb

    head		:: r a -> a
    head		=  Prelude.head . toList

    toList		:: r a -> [a]
    toList		=  foldl (flip (:)) []

    fromList		:: [a] -> r a
    fromList		=  Prelude.foldl (flip insert) rempty

    partition'   	:: (Ord k, Relation r) => (a -> k) -> r a -> r (r a)
    partition' toKey	=  partition toKey >>> snd

    member 		:: (Eq a) => a -> r a -> Bool
    member a		=  projection (==a) >>> foldl (||) False

    foldlKey		:: (Ord k) => (a -> k) -> (b -> a -> b) -> b -> r a -> r b
    foldlKey toKey fn z	=  partition' toKey >>> projection (foldl fn z)

    join 		:: (a -> b -> Bool) ->
                           r a -> r b -> r (a,b)
    join p ra rb	=  foldl fn  rempty ra
        where fn acc el =  foldl fn' rempty rb `union` acc
                 where fn' acc' el' | p el el'  = (el, el') `insert` acc'
                                    | otherwise = acc'
    cartProduct 	:: (TupleUnion a b c) => 
			   r a -> r b -> r c
    cartProduct ra rb	=  projection (uncurry (|+|)) $ join (const (const True)) ra rb

    sortBy 		:: (a -> a -> Ordering) -> r a -> r a
    sortBy pred ra	=  let h = head ra
                           in sortBy pred (selection ((==LT) . (`pred` h)) ra) `union`
                                  (selection  ((==EQ) . (`pred` h)) ra)  `union`
                                  sortBy pred (selection ((==GT) . (`pred` h)) ra)



instance Relation [] where
    foldl		= Prelude.foldl
    rempty		= []
    singleton		= return
    insert		= (:)
    partition		= listPartition

listPartition toKey ra = (toKey, fn ra)
    where fn = map (toKey &&& id) >>>
               List.sortBy  (compare `on` fst) >>>
               List.groupBy ((==)    `on` fst) >>>
               map (map snd)

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

test :: [(Integer, Int)] -> [((Integer, Int), Integer)]
test = sum(fst) `over` partition(snd)
-- test [(1,1),(2,1),(3,2),(4,2),(10,10)]

test1 :: (TupleUnion (Char, Char) b c) => b -> c
test1 = (('a','b') |+|)
-- test1 <$> (pure (\(a, b, c, d)->(a,b)) <*> cartProduct [(1,2),(3,4)] [(5,'a')])

--------------------------------------------------------------------------------

mkAgr :: (Relation r) => (b -> a -> b, b) -> (inp -> a) -> r inp -> b
mkAgr (op,zero) selector = foldl op zero . projection selector

sum 	:: (Num b, Relation r) => (inp -> b) -> r inp -> b
sum 	=  mkAgr ((+), 0)
product :: (Num b, Relation r) => (inp -> b) -> r inp -> b
product =  mkAgr ((*), 1)
count 	:: (Num b, Relation r) => (inp -> a) -> r inp -> b
count 	=  mkAgr (fn, 0)
    where fn acc el = acc + 1

--------------------------------------------------------------------------------

-- deprt >= val 10

(fn1 `less` fn2) a = fn1 a < fn2 a
(fn1 `more` fn2) a = fn1 a < fn2 a
-- ... etc
-- Where  (employee_id `more` val 10)

-- TODO
-- hiding (Ord(..))
-- rewrite >=, <=, == ...
--------------------------------------------------------------------------------

data Employee = Employee { employee_id	 	:: Int
                         , employee_name 	:: String
                         , deprt		:: Int
                         }
employees = [(Employee 1 "1" 10),(Employee 2 "2" 20)]

{-
-- printf-like polymorphism
-- syntax should be
select (employee_id, employee_name, agr(smth) `over` partition(smthElse))
            `from` employees 
            `whereP`  (employee_id <= val 10)
            `groupBy` (OneTuple employee_id)

select (employee_id, employee_name) 
   `whereP` ...

select (count(employee_id), count(employee_name)) 
   `groupBy` (OneTuple employee_id)

-}

-- class TupleApply t inp outp | t -> outp, t -> inp where
select :: (TupleApply t inp outp) => 
          t -> Selection t inp outp
select t =  Selection t

newtype (TupleApply t i o) => Selection t i o = 
    Selection { tupleOfFns	:: t }

class Projectable p where

class (Projectable p) => IFrom a b p 				where from :: a -> b -> p
instance (Projectable p) => IFrom (Selection t i o) (r i) p 	where from   = undefined
instance (Projectable p) => IFrom (Selection t i o) (r (r i)) p where from   = undefined

whereP :: ((r inp -> r outp), r inp) -> 
          (inp -> Bool) -> 
          ((r inp -> r outp), r inp)
whereP = undefined

groupBy :: (Ord gbKey) =>
           ((r inp -> r outp), r inp) ->
           (inp -> gbKey) ->
           ((r inp -> r outp), r inp)
groupBy = undefined


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
{-

data Option gbKey obKey inp outp = Where   { getWhere	:: (inp -> Bool)   }
                                 | GroupBy { getGroupBy :: (inp -> gbKey)  }
                                 | OrderBy { getOrderBy :: (outp -> obKey) }
                                 | Having  { getHaving	:: (outp -> Bool)  }

-- FIXME template haskell?
isOrderBy (OrderBy _) 	= True
isOrderBy _		= False

isGroupBy (GroupBy _) 	= True
isGroupBy _ 		= False

isWhere (Where _) 	= True
isWhere _ 		= False

isHaving (Having _) 	= True
isHaving _ 		= False

countIn :: (Option a b c d -> Bool) -> [Option a b c d] -> Int
sel `countIn` options = count(id) $ filter (==True) $ map sel options


selectV selectors options = selection whereClause	>>> 
                            projectToValues 		>>>
                            sortValues 			>>>
                            selection havingClause
    where 
      (whereClause, groupByClause, orderByCLause, havingClause)
          = parseOptions options
      -- FIXME partitate?
      projectToValues = projection (tapply selectors)
      sortValues = case orderByCLause of
                     Nothing -> id
                     Just fn -> sortBy (compare `on` fn)

parseOptions options 
    | isOrderBy `countIn` options <= 1 &&
      isGroupBy `countIn` options <= 1  = (whereClause, groupByClause, 
                                           orderByCLause, having)
    | otherwise				= error "wrong options"
    where 
      whereClause inp 	= and $ map ($ inp) whereOptions
      whereOptions 	= map getWhere $ filter isWhere options
      groupByClause	= liftM getGroupBy $ List.find isGroupBy options
      orderByCLause	= liftM getOrderBy $ List.find isOrderBy options
      having outp	= and $ map ($ outp) havingOptions
      havingOptions 	= map getHaving $ filter isHaving options
-}
{-
test'' :: [(Int, String)]
test'' =  selectV (employee_id, employee_name) (undefined) employees
-}
