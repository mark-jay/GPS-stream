module Syntax.OrdFunctions
    where

fcompare :: (Ord a) => (inp -> a) -> (inp -> a) -> (inp -> Ordering)
(f1 `fcompare` f2) inp = f1 inp `compare` f2 inp

infix 4 `fLt`, `fNGt`, `fEq`, `fNEq`, `fNLt`, `fGt`
fLt, fNGt, fEq, fNEq, fNLt, fGt
    :: (Ord a) => (inp -> a) -> (inp -> a) -> (inp -> Bool)
(f1 `fLt`  f2) inp = f1 inp <  f2 inp
(f1 `fNGt` f2) inp = f1 inp <= f2 inp
(f1 `fEq`  f2) inp = f1 inp == f2 inp
(f1 `fNEq` f2) inp = f1 inp /= f2 inp
(f1 `fNLt` f2) inp = f1 inp >= f2 inp
(f1 `fGt`  f2) inp = f1 inp >  f2 inp

infixr 2 `fOr`
infixr 3 `fAnd`
fAnd, fOr
    :: (inp -> Bool) -> (inp -> Bool) -> (inp -> Bool)
(f1 `fAnd` f2) inp = f1 inp && f2 inp
(f1 `fOr`  f2) inp = f1 inp || f2 inp

inList :: Eq a => (inp -> a) -> [a] -> (inp -> Bool)
inList f list inp = f inp `elem` list

{-
instance FOrd  where
    (f1 `compare` f2) inp = f1 inp `compare` f2 inp
-}
