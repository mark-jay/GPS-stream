module SqlStream.SqlStream 
    (TableMeta, Query(qOutputTable), Field(..), SQLOrder(ASC, DESC), Acc, Row,
     mkTableMeta, select, from, selectWhere, mkAccum, extractAccum, emptyAcc,
     groupBy, having, orderBy, limit) where

import SqlStream.Fields

import Control.Monad

import Data.Array
import Data.Function
import qualified Data.Map	as Map
import qualified Data.Maybe	as Maybe
import qualified Data.List	as List
import qualified Data.Either	as Either
import Data.Monoid hiding (Sum, Product)

--------------------------------------------------------------------------------
-- implementation

-- Agregators

data Agreg = Count
           | Sum
           | Product
           | Min
           | Max
             deriving(Eq, Show, Read)

-- for foldl
getOp :: Agreg -> ((Field -> Field -> Field), Field)
getOp Sum 	= ((+), 0)
getOp Product 	= ((*), 1)
getOp Min	= (min, FInt $ Just (maxBound :: Int))
getOp Max	= (max, FInt $ Just (minBound :: Int))
getOp Count 	= let counter x _ = x+1
                  in (counter, 0)

type Row 	= [Field]
type ColMeta	= Either String (Agreg, String)
type TableMeta  = [ColMeta]

mkTableMeta :: [String] -> TableMeta
mkTableMeta = map Left

simpleFields :: TableMeta -> [String]
simpleFields = Either.lefts

agregFields :: TableMeta -> [(Agreg, String)]
agregFields  = Either.rights

anyAgreg :: TableMeta -> Bool
anyAgreg = ([] /=) . agregFields

-- FIXME must be memoizing
getRow :: TableMeta -> String -> Int
getRow tm string = 
    Maybe.fromMaybe (error $ "no such column:" ++ string) $ 
                    List.elemIndex (strToColMeta string) tm

strToColMeta :: String -> ColMeta
strToColMeta string = f $ words string
    where f [name] 	  = Left name
          f [agreg, name] = Right (read agreg, name)

--------------------------------------------------------------------------------
-- Query data definition and API

maxLimit = maxBound

data SQLOrder = ASC | DESC
                deriving (Show, Eq)

type Str2Field = String -> Field

data Query a = Query {
      qTypeTo		:: a,
      qInputTable	:: TableMeta,
      qOutputTable	:: TableMeta,
      qWhere		:: [Str2Field -> Bool],
      qRealGroupBy	:: [String],		-- should use qGroupBy as selector
      qHaving		:: [Str2Field -> Bool],
      qOrderBy		:: [(String, SQLOrder)],
      qLimit		:: (Int, Int)
    }

qGroupBy :: Query a -> [String]
qGroupBy q | (qRealGroupBy q) /= [] 		= qRealGroupBy q
           | (not $ anyAgreg $ qOutputTable q) 	= simpleFields $ qInputTable q
           | otherwise				= qRealGroupBy q

instance Monad Query where
    return a	= Query a [] [] [] [] [] [] (0, maxLimit)
    q1 >>= pq2	= let (Query tt1 itm1 otm1 w1 gb1 h1 ob1 (min1, max1)) = q1
                      (Query tt2 itm2 otm2 w2 gb2 h2 ob2 (min2, max2)) = pq2 tt1
                  in Query tt2 (itm1 ++ itm2) (otm1 ++ otm2) (w1 ++ w2) (gb1 ++ gb2)
                           (h1 ++ h2) (ob1 ++ ob2) (min1 `max` min2, max1 `min` max2)

--------------------------------------------------------------------------------
-- Query constructors

-- FIXME dunno how

select :: [String] -> Query ()
select strings = Query () [] (map strToColMeta strings)  [] [] [] [] (0, maxLimit)

from :: TableMeta -> Query ()
from itm = Query () itm [] [] [] [] [] (0, maxLimit)

selectWhere :: [Str2Field -> Bool] -> Query ()
selectWhere sw = Query () [] [] sw [] [] [] (0, maxLimit)

groupBy :: [String] -> Query ()
groupBy gb = Query () [] [] [] gb [] [] (0, maxLimit)

having :: [Str2Field -> Bool] -> Query ()
having h = Query () [] [] [] [] h [] (0, maxLimit)

orderBy :: [(String, SQLOrder)] -> Query ()
orderBy ob = Query () [] [] [] [] [] ob (0, maxLimit)

limit :: Int -> Query ()
limit l = limitFromTo (0, l)

limitFromTo :: (Int, Int) -> Query ()
limitFromTo l = Query () [] [] [] [] [] [] l

--------------------------------------------------------------------------------

type Acc = Map.Map [Field] [Row]

emptyAcc :: Map.Map [Field] [Row]
emptyAcc  = Map.empty

mkAccum :: Query () -> (Acc -> Row -> Acc)
mkAccum q@(Query _ itm _ w _ _ _ _) acc row = fn where

    fn  | wherePred	= Map.alter updater gbKeys acc
        | otherwise	= acc

    str2Field :: Str2Field
    str2Field string = row !! getRow itm string

    -- sql where
    wherePred :: Bool
    wherePred = and $ map ( $ str2Field) w

    -- group-by-key-fields
    gbKeys :: [Field]
    gbKeys = map str2Field $ qGroupBy q

    updater :: Maybe [Row] -> Maybe [Row]
    updater Nothing   = Just $ [row]
    updater (Just rs) = Just $ row : rs

extractAccum :: Query () -> Acc -> [Row]
extractAccum (Query _ itm otm w _ h ob (min, max)) = 
    take (max - min) . drop min . List.sortBy sortPred . filter isHaving . accToTable 
        where 
          accToTable :: Acc -> [Row]
          accToTable = map (foldRows . snd) . Map.toList where
              foldRows :: [Row] -> Row
              foldRows [] = error "extractAccum: empty list"
              foldRows rows = map (f rows) otm

              tableToCol rows name = map (!! getRow itm name) rows

              f :: [Row] -> Either String (Agreg, String) -> Field
              f rows (Left  name)	   = tableToCol rows name !! 0
              f rows (Right (agreg, name)) = let col 	    = tableToCol rows name
                                                 (fn, zero) = getOp agreg
                                             in foldl fn zero col

          isHaving :: Row -> Bool
          isHaving row = and $ map ($ str2Field) h where
              str2Field string = row !! getRow otm string

          sortPred :: Row -> Row -> Ordering
          sortPred  = sortPred' ob where
              sortPred' :: [(String, SQLOrder)] -> Row -> Row -> Ordering
              sortPred' [] = mempty
              sortPred' ((string, order)  : t) = mappend (cmp `on` selector)
                                                         (sortPred' t)
                  where cmp | order == ASC	= compare
                            | otherwise		= flip compare
                        selector :: Row -> Field
                        selector = (!! getRow otm string)
