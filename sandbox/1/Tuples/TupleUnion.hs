{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module TupleUnion(TupleUnion, tunion, (|+|))
	where

import DataDef

class TupleUnion a b c | a b -> c where
        -- Minimal complete definition:
        --      tunion
	tunion :: a -> b -> c
	(|+|)  :: a -> b -> c
	(|+|)  =  tunion

infixr 5 |+|

-- generated code 'Tools.TuplesGen.Main' was called with args '10 union'
instance TupleUnion (OneTuple a1) (OneTuple a2) (a1,a2) where
    tunion (OneTuple a1) (OneTuple a2) = (a1,a2)
instance TupleUnion (OneTuple a1) (a2,a3) (a1,a2,a3) where
    tunion (OneTuple a1) (a2,a3) = (a1,a2,a3)
instance TupleUnion (OneTuple a1) (a2,a3,a4) (a1,a2,a3,a4) where
    tunion (OneTuple a1) (a2,a3,a4) = (a1,a2,a3,a4)
instance TupleUnion (OneTuple a1) (a2,a3,a4,a5) (a1,a2,a3,a4,a5) where
    tunion (OneTuple a1) (a2,a3,a4,a5) = (a1,a2,a3,a4,a5)
instance TupleUnion (OneTuple a1) (a2,a3,a4,a5,a6) (a1,a2,a3,a4,a5,a6) where
    tunion (OneTuple a1) (a2,a3,a4,a5,a6) = (a1,a2,a3,a4,a5,a6)
instance TupleUnion (OneTuple a1) (a2,a3,a4,a5,a6,a7) (a1,a2,a3,a4,a5,a6,a7) where
    tunion (OneTuple a1) (a2,a3,a4,a5,a6,a7) = (a1,a2,a3,a4,a5,a6,a7)
instance TupleUnion (OneTuple a1) (a2,a3,a4,a5,a6,a7,a8) (a1,a2,a3,a4,a5,a6,a7,a8) where
    tunion (OneTuple a1) (a2,a3,a4,a5,a6,a7,a8) = (a1,a2,a3,a4,a5,a6,a7,a8)
instance TupleUnion (OneTuple a1) (a2,a3,a4,a5,a6,a7,a8,a9) (a1,a2,a3,a4,a5,a6,a7,a8,a9) where
    tunion (OneTuple a1) (a2,a3,a4,a5,a6,a7,a8,a9) = (a1,a2,a3,a4,a5,a6,a7,a8,a9)
instance TupleUnion (OneTuple a1) (a2,a3,a4,a5,a6,a7,a8,a9,a10) (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) where
    tunion (OneTuple a1) (a2,a3,a4,a5,a6,a7,a8,a9,a10) = (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10)
instance TupleUnion (a1,a2) (OneTuple a3) (a1,a2,a3) where
    tunion (a1,a2) (OneTuple a3) = (a1,a2,a3)
instance TupleUnion (a1,a2) (a3,a4) (a1,a2,a3,a4) where
    tunion (a1,a2) (a3,a4) = (a1,a2,a3,a4)
instance TupleUnion (a1,a2) (a3,a4,a5) (a1,a2,a3,a4,a5) where
    tunion (a1,a2) (a3,a4,a5) = (a1,a2,a3,a4,a5)
instance TupleUnion (a1,a2) (a3,a4,a5,a6) (a1,a2,a3,a4,a5,a6) where
    tunion (a1,a2) (a3,a4,a5,a6) = (a1,a2,a3,a4,a5,a6)
instance TupleUnion (a1,a2) (a3,a4,a5,a6,a7) (a1,a2,a3,a4,a5,a6,a7) where
    tunion (a1,a2) (a3,a4,a5,a6,a7) = (a1,a2,a3,a4,a5,a6,a7)
instance TupleUnion (a1,a2) (a3,a4,a5,a6,a7,a8) (a1,a2,a3,a4,a5,a6,a7,a8) where
    tunion (a1,a2) (a3,a4,a5,a6,a7,a8) = (a1,a2,a3,a4,a5,a6,a7,a8)
instance TupleUnion (a1,a2) (a3,a4,a5,a6,a7,a8,a9) (a1,a2,a3,a4,a5,a6,a7,a8,a9) where
    tunion (a1,a2) (a3,a4,a5,a6,a7,a8,a9) = (a1,a2,a3,a4,a5,a6,a7,a8,a9)
instance TupleUnion (a1,a2) (a3,a4,a5,a6,a7,a8,a9,a10) (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) where
    tunion (a1,a2) (a3,a4,a5,a6,a7,a8,a9,a10) = (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10)
instance TupleUnion (a1,a2,a3) (OneTuple a4) (a1,a2,a3,a4) where
    tunion (a1,a2,a3) (OneTuple a4) = (a1,a2,a3,a4)
instance TupleUnion (a1,a2,a3) (a4,a5) (a1,a2,a3,a4,a5) where
    tunion (a1,a2,a3) (a4,a5) = (a1,a2,a3,a4,a5)
instance TupleUnion (a1,a2,a3) (a4,a5,a6) (a1,a2,a3,a4,a5,a6) where
    tunion (a1,a2,a3) (a4,a5,a6) = (a1,a2,a3,a4,a5,a6)
instance TupleUnion (a1,a2,a3) (a4,a5,a6,a7) (a1,a2,a3,a4,a5,a6,a7) where
    tunion (a1,a2,a3) (a4,a5,a6,a7) = (a1,a2,a3,a4,a5,a6,a7)
instance TupleUnion (a1,a2,a3) (a4,a5,a6,a7,a8) (a1,a2,a3,a4,a5,a6,a7,a8) where
    tunion (a1,a2,a3) (a4,a5,a6,a7,a8) = (a1,a2,a3,a4,a5,a6,a7,a8)
instance TupleUnion (a1,a2,a3) (a4,a5,a6,a7,a8,a9) (a1,a2,a3,a4,a5,a6,a7,a8,a9) where
    tunion (a1,a2,a3) (a4,a5,a6,a7,a8,a9) = (a1,a2,a3,a4,a5,a6,a7,a8,a9)
instance TupleUnion (a1,a2,a3) (a4,a5,a6,a7,a8,a9,a10) (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) where
    tunion (a1,a2,a3) (a4,a5,a6,a7,a8,a9,a10) = (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10)
instance TupleUnion (a1,a2,a3,a4) (OneTuple a5) (a1,a2,a3,a4,a5) where
    tunion (a1,a2,a3,a4) (OneTuple a5) = (a1,a2,a3,a4,a5)
instance TupleUnion (a1,a2,a3,a4) (a5,a6) (a1,a2,a3,a4,a5,a6) where
    tunion (a1,a2,a3,a4) (a5,a6) = (a1,a2,a3,a4,a5,a6)
instance TupleUnion (a1,a2,a3,a4) (a5,a6,a7) (a1,a2,a3,a4,a5,a6,a7) where
    tunion (a1,a2,a3,a4) (a5,a6,a7) = (a1,a2,a3,a4,a5,a6,a7)
instance TupleUnion (a1,a2,a3,a4) (a5,a6,a7,a8) (a1,a2,a3,a4,a5,a6,a7,a8) where
    tunion (a1,a2,a3,a4) (a5,a6,a7,a8) = (a1,a2,a3,a4,a5,a6,a7,a8)
instance TupleUnion (a1,a2,a3,a4) (a5,a6,a7,a8,a9) (a1,a2,a3,a4,a5,a6,a7,a8,a9) where
    tunion (a1,a2,a3,a4) (a5,a6,a7,a8,a9) = (a1,a2,a3,a4,a5,a6,a7,a8,a9)
instance TupleUnion (a1,a2,a3,a4) (a5,a6,a7,a8,a9,a10) (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) where
    tunion (a1,a2,a3,a4) (a5,a6,a7,a8,a9,a10) = (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10)
instance TupleUnion (a1,a2,a3,a4,a5) (OneTuple a6) (a1,a2,a3,a4,a5,a6) where
    tunion (a1,a2,a3,a4,a5) (OneTuple a6) = (a1,a2,a3,a4,a5,a6)
instance TupleUnion (a1,a2,a3,a4,a5) (a6,a7) (a1,a2,a3,a4,a5,a6,a7) where
    tunion (a1,a2,a3,a4,a5) (a6,a7) = (a1,a2,a3,a4,a5,a6,a7)
instance TupleUnion (a1,a2,a3,a4,a5) (a6,a7,a8) (a1,a2,a3,a4,a5,a6,a7,a8) where
    tunion (a1,a2,a3,a4,a5) (a6,a7,a8) = (a1,a2,a3,a4,a5,a6,a7,a8)
instance TupleUnion (a1,a2,a3,a4,a5) (a6,a7,a8,a9) (a1,a2,a3,a4,a5,a6,a7,a8,a9) where
    tunion (a1,a2,a3,a4,a5) (a6,a7,a8,a9) = (a1,a2,a3,a4,a5,a6,a7,a8,a9)
instance TupleUnion (a1,a2,a3,a4,a5) (a6,a7,a8,a9,a10) (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) where
    tunion (a1,a2,a3,a4,a5) (a6,a7,a8,a9,a10) = (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10)
instance TupleUnion (a1,a2,a3,a4,a5,a6) (OneTuple a7) (a1,a2,a3,a4,a5,a6,a7) where
    tunion (a1,a2,a3,a4,a5,a6) (OneTuple a7) = (a1,a2,a3,a4,a5,a6,a7)
instance TupleUnion (a1,a2,a3,a4,a5,a6) (a7,a8) (a1,a2,a3,a4,a5,a6,a7,a8) where
    tunion (a1,a2,a3,a4,a5,a6) (a7,a8) = (a1,a2,a3,a4,a5,a6,a7,a8)
instance TupleUnion (a1,a2,a3,a4,a5,a6) (a7,a8,a9) (a1,a2,a3,a4,a5,a6,a7,a8,a9) where
    tunion (a1,a2,a3,a4,a5,a6) (a7,a8,a9) = (a1,a2,a3,a4,a5,a6,a7,a8,a9)
instance TupleUnion (a1,a2,a3,a4,a5,a6) (a7,a8,a9,a10) (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) where
    tunion (a1,a2,a3,a4,a5,a6) (a7,a8,a9,a10) = (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10)
instance TupleUnion (a1,a2,a3,a4,a5,a6,a7) (OneTuple a8) (a1,a2,a3,a4,a5,a6,a7,a8) where
    tunion (a1,a2,a3,a4,a5,a6,a7) (OneTuple a8) = (a1,a2,a3,a4,a5,a6,a7,a8)
instance TupleUnion (a1,a2,a3,a4,a5,a6,a7) (a8,a9) (a1,a2,a3,a4,a5,a6,a7,a8,a9) where
    tunion (a1,a2,a3,a4,a5,a6,a7) (a8,a9) = (a1,a2,a3,a4,a5,a6,a7,a8,a9)
instance TupleUnion (a1,a2,a3,a4,a5,a6,a7) (a8,a9,a10) (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) where
    tunion (a1,a2,a3,a4,a5,a6,a7) (a8,a9,a10) = (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10)
instance TupleUnion (a1,a2,a3,a4,a5,a6,a7,a8) (OneTuple a9) (a1,a2,a3,a4,a5,a6,a7,a8,a9) where
    tunion (a1,a2,a3,a4,a5,a6,a7,a8) (OneTuple a9) = (a1,a2,a3,a4,a5,a6,a7,a8,a9)
instance TupleUnion (a1,a2,a3,a4,a5,a6,a7,a8) (a9,a10) (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) where
    tunion (a1,a2,a3,a4,a5,a6,a7,a8) (a9,a10) = (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10)
instance TupleUnion (a1,a2,a3,a4,a5,a6,a7,a8,a9) (OneTuple a10) (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) where
    tunion (a1,a2,a3,a4,a5,a6,a7,a8,a9) (OneTuple a10) = (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10)
