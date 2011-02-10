{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies #-}
-- {-# LANGUAGE UndecidableInstances #-}
	

module Main where

import TupleApply
import TupleJoin
import Relation
import TupleUnion

import Control.Applicative
import Control.Arrow
import Control.Monad hiding (join)

import Data.Tuple.All

import qualified Data.Maybe as Maybe

import Prelude hiding (foldl, sum, head)
import qualified Prelude

--------------------------------------------------------------------------------

mkAgr  :: (Relation r) => (outp -> a -> outp, outp) -> (inp -> a) -> r (k,inp) -> r (k,outp)
mkAgr (op,zero) selector = singleton . foldl op' zero' . projection (id***selector)
    where op' (_,acc) (k,el) = (k,acc `op` el)
          zero' = (undefined, zero)

sum 	:: (Num outp, Relation r) => (inp -> outp) -> r (k,inp) -> r (k,outp)
sum 	=  mkAgr ((+), 0)
product :: (Num outp, Relation r) => (inp -> outp) -> r (k,inp) -> r (k,outp)
product =  mkAgr ((*), 1)
count 	:: (Num outp, Relation r) => (inp -> a) -> r (k,inp) -> r (k,outp)
count 	=  mkAgr (fn, 0)
    where fn acc el = acc + 1

overK :: (Relation r, Eq k) =>
         (r a -> r b) -> Partition r k a -> r a -> r b
agr `overK` part = flattenR . projection snd . (agr `over` part)

{-
overK :: (Relation r, Eq k1) =>
         (r (k,a) -> r (k,b)) -> (r (k,a) -> ((k,a) -> k1, r (r (k,a)))) -> r (k,a) -> r (k,b)
(agr `overK'` part) ra = flattenR . projection snd . (agr `over` part) $ ra
(agr `over` part) $ ra :: r (k1, (r (k,b)))
-}


partitionK :: (Relation r, Ord k1) => (a -> k1) -> Partition r k1 (k, a)
partitionK fn = partition fn'
    where fn' = fn . snd
--sum(employee_id) `overK` partitionK(employee_name) $ employees

-- just value
v	:: (Relation r) => (a -> b) -> r (k, a) -> r (k, b)
v	=  projection . (id***)

--------------------------------------------------------------------------------

-- deprt >= val 10

(fn1 `less` fn2) a = fn1 a < fn2 a
(fn1 `more` fn2) a = fn1 a < fn2 a
-- ... etc
-- Where  (employee_id `more` val 10)

--------------------------------------------------------------------------------

data Employee = Employee { employee_id	 	:: Int
                         , employee_name 	:: String
                         , deprt		:: Int
                         }
              deriving(Show, Read, Eq, Ord)

newtype EmplPK = EmplPK Int
    deriving(Show, Read, Eq, Ord)
toPK = EmplPK . employee_id

employees' = [(Employee 1 "1" 10), (Employee 2 "2" 20), (Employee 3 "3" 20)]
employees  = map (toPK &&& id) employees'

-- FIXME how to coerce (a -> a1, a -> a1 .. a->an) to (a -> (a1,...an))
v'	:: (a -> b) -> [(EmplPK, a)] -> [(EmplPK, b)]
v' 	=  v

mkAgrE  :: (outp -> a -> outp, outp) -> (inp -> a) -> [(EmplPK, inp)] -> [(EmplPK, outp)]
mkAgrE  = mkAgr

sumE    :: (inp -> Int) -> [(EmplPK, inp)] -> [(EmplPK, Int)]
sumE 	=  mkAgrE ((+), 0)

--------------------
-- FIXME
select :: (Relation r, TupleJoin tuplesOfR (r b), TupleApply sel (r a) tuplesOfR) => 
          sel -> r a -> r b
select sel = tjoin' . tapply sel

sel `from` ra = (sel, ra)

_where :: (a, b)-> c -> (a,b,c)
q `_where` p = q |+ p

run1 tuple = (sel1 tuple) (sel2 tuple)
run2 tuple = (sel1 tuple) $ (selection (sel3 tuple) (sel2 tuple))
{-
run1 $ select (v' employee_id,v' employee_id, sumE(employee_id) `overK` partitionK(employee_name)) `from` employees
run2 $ select (v' employee_id,v' employee_id, sumE(employee_id) `overK` partitionK(deprt)) `from` employees `_where` (const True)
...etc
-}

