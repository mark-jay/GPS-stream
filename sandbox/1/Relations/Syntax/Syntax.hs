{-# LANGUAGE GADTs #-}
module Syntax.Syntax where

import Relation
import RelMap
import Tuple.TupleApply
import Syntax.Window

import Syntax.OrdFunctions
import Prelude hiding(Ord(..), Eq(..), (&&), (||))
import qualified Prelude as Prel
--------------------------------------------------------------------------------
-- Options datatype,  constructors, selectors
data Option t inp outp where
    Where :: (inp -> Bool) -> Option t inp outp
    Tuple :: TupleApply t inp outp => t -> Option t inp outp
    -- ^ Use monomorphic function type in tuple

oWhere (Where w) = w
oTuple (Tuple t) = t

isTuple (Tuple _) = True
isTuple _	  = False

isWhere (Where _) = True
isWhere _	  = False

--------------------------------------------------------------------------------
-- parsing options
parseOpts :: (TupleApply t inp outp) =>
             [Option t inp outp] -> (inp -> Bool, t)
parseOpts = tapply (getWhere, getTuple)

-- get where clause
getWhere :: [Option t inp outp] -> (inp -> Bool)
getWhere = getWhere' . map oWhere . filter isWhere
    where getWhere' fns inp = and [(f inp) | f <- fns]

getTuple :: TupleApply t inp outp =>
            [Option t inp outp] -> t
getTuple 	   = f . filter isTuple
    where f [Tuple t] 	= t
          f _		= error "select must contain exactly 1 tuple to select"






{-
let t = (zip [0..10] [1..11])
select [ Tuple (fst', snd', fst')
       ]
  (t `over` wind [byTime 1000])
-}

--------------------------------------------------------------------------------
-- selects, etc
select :: (Relation r, TupleApply t inp outp) =>
          [Option t inp outp] -> r inp -> r outp
select opts = projection (tapply tuple) . selection whereP
    where (whereP, tuple) = parseOpts opts


-- Utils
c = const

--------------------------------------------------------------------------------
-- remove it

{-
select [ Tuple (fst', snd', fst')
       , Where ( (fst' `fEq` c 1) `fOr`
                 (fst' `fEq` c 2)
               )
       , Where (fst' `inList` [0..10])
       ]
  (zip [0..10] [1..11])
-}
fst' :: (Int, Int) -> Int
fst' = fst

snd' :: (Int, Int) -> Int
snd' = snd

-- select [Where (fst == c 1)]
