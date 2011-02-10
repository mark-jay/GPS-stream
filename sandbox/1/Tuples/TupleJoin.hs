{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts, TupleSections #-}
module TupleJoin where
-- (r (k,a),r (k,b)...) -> r (k, (a,b,..))

import Relation
import Data.Function
import DataDef

import TupleUnion

import Control.Arrow

class TupleJoin a b | a -> b where
    tjoin' :: a -> b

eqFst :: (Eq k) => (k, a) -> (k, b) -> Bool
eqFst ka kb = fst ka == fst kb

sndMkPair :: (k, a1) -> (k, a2) -> (k, (a1,a2))
sndMkPair (k, a1) (_, a2) = (k, (a1, a2))

sndInsTuple :: (TupleUnion (OneTuple el) t t') => 
               (k, el) -> (k, t) -> (k, t')
sndInsTuple (k, a) (_, t) = (k, (a +| t))

joinByKey :: (Eq k, Relation r) =>
     ((k, a) -> (k, b) -> (k, c)) -> r (k, a) -> r (k, b) -> r (k, c)
joinByKey = joinS eqFst

instance (Relation r, Eq k) => TupleJoin 
    (OneTuple (r (k, a1))) 
    (r (k, (OneTuple a1)))
        where
    tjoin' (OneTuple rka1) = projection (id *** OneTuple) rka1

-- generated code 'Tools.TuplesGen.Main' was called with args 'join 10'
instance (Relation r, Eq k) => TupleJoin
    (r (k, a1), r (k, a2))
    (r (k, (a1, a2)))
        where
    tjoin' (rka1, rka2) = joinByKey sndMkPair rka1 rka2
instance (Relation r, Eq k) => TupleJoin
    (r (k, a1), r (k, a2), r (k, a3))
    (r (k, (a1, a2, a3)))
        where
    tjoin' (rka1, rka2, rka3) = joinByKey sndInsTuple rka1 $ joinByKey sndMkPair rka2 rka3
instance (Relation r, Eq k) => TupleJoin
    (r (k, a1), r (k, a2), r (k, a3), r (k, a4))
    (r (k, (a1, a2, a3, a4)))
        where
    tjoin' (rka1, rka2, rka3, rka4) = joinByKey sndInsTuple rka1 $ joinByKey sndInsTuple rka2 $ joinByKey sndMkPair rka3 rka4
instance (Relation r, Eq k) => TupleJoin
    (r (k, a1), r (k, a2), r (k, a3), r (k, a4), r (k, a5))
    (r (k, (a1, a2, a3, a4, a5)))
        where
    tjoin' (rka1, rka2, rka3, rka4, rka5) = joinByKey sndInsTuple rka1 $ joinByKey sndInsTuple rka2 $ joinByKey sndInsTuple rka3 $ joinByKey sndMkPair rka4 rka5
instance (Relation r, Eq k) => TupleJoin
    (r (k, a1), r (k, a2), r (k, a3), r (k, a4), r (k, a5), r (k, a6))
    (r (k, (a1, a2, a3, a4, a5, a6)))
        where
    tjoin' (rka1, rka2, rka3, rka4, rka5, rka6) = joinByKey sndInsTuple rka1 $ joinByKey sndInsTuple rka2 $ joinByKey sndInsTuple rka3 $ joinByKey sndInsTuple rka4 $ joinByKey sndMkPair rka5 rka6
instance (Relation r, Eq k) => TupleJoin
    (r (k, a1), r (k, a2), r (k, a3), r (k, a4), r (k, a5), r (k, a6), r (k, a7))
    (r (k, (a1, a2, a3, a4, a5, a6, a7)))
        where
    tjoin' (rka1, rka2, rka3, rka4, rka5, rka6, rka7) = joinByKey sndInsTuple rka1 $ joinByKey sndInsTuple rka2 $ joinByKey sndInsTuple rka3 $ joinByKey sndInsTuple rka4 $ joinByKey sndInsTuple rka5 $ joinByKey sndMkPair rka6 rka7
instance (Relation r, Eq k) => TupleJoin
    (r (k, a1), r (k, a2), r (k, a3), r (k, a4), r (k, a5), r (k, a6), r (k, a7), r (k, a8))
    (r (k, (a1, a2, a3, a4, a5, a6, a7, a8)))
        where
    tjoin' (rka1, rka2, rka3, rka4, rka5, rka6, rka7, rka8) = joinByKey sndInsTuple rka1 $ joinByKey sndInsTuple rka2 $ joinByKey sndInsTuple rka3 $ joinByKey sndInsTuple rka4 $ joinByKey sndInsTuple rka5 $ joinByKey sndInsTuple rka6 $ joinByKey sndMkPair rka7 rka8
instance (Relation r, Eq k) => TupleJoin
    (r (k, a1), r (k, a2), r (k, a3), r (k, a4), r (k, a5), r (k, a6), r (k, a7), r (k, a8), r (k, a9))
    (r (k, (a1, a2, a3, a4, a5, a6, a7, a8, a9)))
        where
    tjoin' (rka1, rka2, rka3, rka4, rka5, rka6, rka7, rka8, rka9) = joinByKey sndInsTuple rka1 $ joinByKey sndInsTuple rka2 $ joinByKey sndInsTuple rka3 $ joinByKey sndInsTuple rka4 $ joinByKey sndInsTuple rka5 $ joinByKey sndInsTuple rka6 $ joinByKey sndInsTuple rka7 $ joinByKey sndMkPair rka8 rka9
instance (Relation r, Eq k) => TupleJoin
    (r (k, a1), r (k, a2), r (k, a3), r (k, a4), r (k, a5), r (k, a6), r (k, a7), r (k, a8), r (k, a9), r (k, a10))
    (r (k, (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)))
        where
    tjoin' (rka1, rka2, rka3, rka4, rka5, rka6, rka7, rka8, rka9, rka10) = joinByKey sndInsTuple rka1 $ joinByKey sndInsTuple rka2 $ joinByKey sndInsTuple rka3 $ joinByKey sndInsTuple rka4 $ joinByKey sndInsTuple rka5 $ joinByKey sndInsTuple rka6 $ joinByKey sndInsTuple rka7 $ joinByKey sndInsTuple rka8 $ joinByKey sndMkPair rka9 rka10