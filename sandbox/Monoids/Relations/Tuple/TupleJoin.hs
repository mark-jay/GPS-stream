{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts, TupleSections #-}
module Tuple.TupleJoin where
-- (r (Row k a),r (Row k b)...) -> r (Row k  (a,b,..))


{-
import Relation
import Data.Function
import Data.Tuple.All

import Tuple.TupleConcat

import Control.Arrow

import Data.Function

class TupleJoin a b | a -> b where
    tjoin :: a -> b

eqKey :: (Eq k) => Row k a -> Row k b -> Bool
eqKey r1 r2 = getKey r1 == getKey r2

sndMkPair :: Row k a1 -> Row k a2 -> Row k (a1, a2)
sndMkPair (Row k a1) (Row _ a2) = Row k (a1, a2)

sndInsTuple :: (TupleUnion (OneTuple el) t t') => 
               Row k el -> Row k t -> Row k t'
sndInsTuple (Row k a) (Row _ t) = Row k (a +| t)

joinByKey :: (Eq k, Relation r) =>
     ((Row k a) -> (Row k b) -> (Row k c)) -> r (Row k a) -> r (Row k b) -> r (Row k c)
joinByKey = joinG eqKey

instance (Relation r, Eq k) => TupleJoin 
    (OneTuple (r (Row k a1)))
    (r (Row k (OneTuple a1)))
        where
    tjoin (OneTuple rka1) = projection (mapRow OneTuple) rka1

-- generated code 'Tools.TuplesGen.Main' was called with args 'join 10'
instance (Relation r, Eq k) => TupleJoin
    (r (Row k a1), r (Row k a2))
    (r (Row k (a1, a2)))
        where
    tjoin (rka1, rka2) = joinByKey sndMkPair rka1 rka2
instance (Relation r, Eq k) => TupleJoin
    (r (Row k a1), r (Row k a2), r (Row k a3))
    (r (Row k (a1, a2, a3)))
        where
    tjoin (rka1, rka2, rka3) = joinByKey sndInsTuple rka1 $ joinByKey sndMkPair rka2 rka3
instance (Relation r, Eq k) => TupleJoin
    (r (Row k a1), r (Row k a2), r (Row k a3), r (Row k a4))
    (r (Row k (a1, a2, a3, a4)))
        where
    tjoin (rka1, rka2, rka3, rka4) = joinByKey sndInsTuple rka1 $ joinByKey sndInsTuple rka2 $ joinByKey sndMkPair rka3 rka4
instance (Relation r, Eq k) => TupleJoin
    (r (Row k a1), r (Row k a2), r (Row k a3), r (Row k a4), r (Row k a5))
    (r (Row k (a1, a2, a3, a4, a5)))
        where
    tjoin (rka1, rka2, rka3, rka4, rka5) = joinByKey sndInsTuple rka1 $ joinByKey sndInsTuple rka2 $ joinByKey sndInsTuple rka3 $ joinByKey sndMkPair rka4 rka5
instance (Relation r, Eq k) => TupleJoin
    (r (Row k a1), r (Row k a2), r (Row k a3), r (Row k a4), r (Row k a5), r (Row k a6))
    (r (Row k (a1, a2, a3, a4, a5, a6)))
        where
    tjoin (rka1, rka2, rka3, rka4, rka5, rka6) = joinByKey sndInsTuple rka1 $ joinByKey sndInsTuple rka2 $ joinByKey sndInsTuple rka3 $ joinByKey sndInsTuple rka4 $ joinByKey sndMkPair rka5 rka6
instance (Relation r, Eq k) => TupleJoin
    (r (Row k a1), r (Row k a2), r (Row k a3), r (Row k a4), r (Row k a5), r (Row k a6), r (Row k a7))
    (r (Row k (a1, a2, a3, a4, a5, a6, a7)))
        where
    tjoin (rka1, rka2, rka3, rka4, rka5, rka6, rka7) = joinByKey sndInsTuple rka1 $ joinByKey sndInsTuple rka2 $ joinByKey sndInsTuple rka3 $ joinByKey sndInsTuple rka4 $ joinByKey sndInsTuple rka5 $ joinByKey sndMkPair rka6 rka7
instance (Relation r, Eq k) => TupleJoin
    (r (Row k a1), r (Row k a2), r (Row k a3), r (Row k a4), r (Row k a5), r (Row k a6), r (Row k a7), r (Row k a8))
    (r (Row k (a1, a2, a3, a4, a5, a6, a7, a8)))
        where
    tjoin (rka1, rka2, rka3, rka4, rka5, rka6, rka7, rka8) = joinByKey sndInsTuple rka1 $ joinByKey sndInsTuple rka2 $ joinByKey sndInsTuple rka3 $ joinByKey sndInsTuple rka4 $ joinByKey sndInsTuple rka5 $ joinByKey sndInsTuple rka6 $ joinByKey sndMkPair rka7 rka8
instance (Relation r, Eq k) => TupleJoin
    (r (Row k a1), r (Row k a2), r (Row k a3), r (Row k a4), r (Row k a5), r (Row k a6), r (Row k a7), r (Row k a8), r (Row k a9))
    (r (Row k (a1, a2, a3, a4, a5, a6, a7, a8, a9)))
        where
    tjoin (rka1, rka2, rka3, rka4, rka5, rka6, rka7, rka8, rka9) = joinByKey sndInsTuple rka1 $ joinByKey sndInsTuple rka2 $ joinByKey sndInsTuple rka3 $ joinByKey sndInsTuple rka4 $ joinByKey sndInsTuple rka5 $ joinByKey sndInsTuple rka6 $ joinByKey sndInsTuple rka7 $ joinByKey sndMkPair rka8 rka9
instance (Relation r, Eq k) => TupleJoin
    (r (Row k a1), r (Row k a2), r (Row k a3), r (Row k a4), r (Row k a5), r (Row k a6), r (Row k a7), r (Row k a8), r (Row k a9), r (Row k a10))
    (r (Row k (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)))
        where
    tjoin (rka1, rka2, rka3, rka4, rka5, rka6, rka7, rka8, rka9, rka10) = joinByKey sndInsTuple rka1 $ joinByKey sndInsTuple rka2 $ joinByKey sndInsTuple rka3 $ joinByKey sndInsTuple rka4 $ joinByKey sndInsTuple rka5 $ joinByKey sndInsTuple rka6 $ joinByKey sndInsTuple rka7 $ joinByKey sndInsTuple rka8 $ joinByKey sndMkPair rka9 rka10
-}