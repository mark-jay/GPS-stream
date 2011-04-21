{-# LANGUAGE GADTs #-}
module Syntax.Syntax where

import Relation
import Timable
import Window
import RelMap
import Tuple.TupleApply
import Syntax.Window

import Data.Time
import Data.Monoid
import Data.Time.Clock

import Syntax.OrdFunctions
import Prelude hiding(Ord(..), Eq(..), (&&), (||))
import qualified Prelude as Prel
--------------------------------------------------------------------------------
-- QClauses datatype,  constructors, selectors
data QClause t inp outp where
    Where :: (inp -> Bool) -> QClause t inp outp
    Tuple :: TupleApply t inp outp => t -> QClause t inp outp
    -- ^ Use monomorphic function type in tuple

oWhere (Where w) = w
oTuple (Tuple t) = t

isTuple (Tuple _) = True
isTuple _	  = False

isWhere (Where _) = True
isWhere _	  = False

data StreamLim = SLTimeLim  NominalDiffTime
               | SLCountLim Int

--------------------------------------------------------------------------------
-- parsing options
parseOpts :: (TupleApply t inp outp) =>
             [QClause t inp outp] -> (inp -> Bool, t)
parseOpts = tapply (getWhere, getTuple)

-- get where clause
getWhere :: [QClause t inp outp] -> (inp -> Bool)
getWhere = getWhere' . map oWhere . filter isWhere
    where getWhere' fns inp = and [(f inp) | f <- fns]

getTuple :: TupleApply t inp outp =>
            [QClause t inp outp] -> t
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

{-
select :: (TupleApply t inp outp) =>
          [QClause t inp outp] -> RelMap inp outp
select opts = projection (tapply tuple) . selection whereP
    where (whereP, tuple) = parseOpts opts
-}

streamLimsToRM :: (Timable a) => [StreamLim] -> RelMap a a
streamLimsToRM = uncurry windowedFn . foldl f (Nothing, Nothing)
    where f (count, time) (SLTimeLim  t) = (count, Just t)
          f (count, time) (SLCountLim c) = (Just c,  time)

selectStream :: (TupleApply t inp outp, Timable inp) =>
                [StreamLim] -> [QClause t inp outp] -> RelMap inp outp
selectStream lims opts = mkRelMap (mkMapper $ parseOpts opts) Seq.empty
    where 
      mkMapper (whereP, tuple) inp = mkRelMap (mkMapper popts' t') outp
          where
            tapply tuple inp
            outp

{-
let t  = (zip [0..10] [1..11])
in $( select [| 
           [SLCountLim 1000]
           [ Tuple (fst', snd', fst', sum(fst') `over` patition(snd', t))
           , Where (fst' `inList` [0..10])
           ]
	t
    |])

fst' :: RelMap (Int, Int) Int
fst' = lastRow fst

-}


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
