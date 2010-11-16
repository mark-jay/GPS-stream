module Main where
-- TODO
import qualified 	Data.ByteString.Lazy 		as L
import 			Data.Char (isSpace)
import qualified 	Data.ByteString.Lazy.Char8 	as L8
import Data.Data

main :: IO ()
main = undefined

data PrpDist = All | Distinct

data Query = Select PrpDist
                    SelectExpr
                    From
                    Where
                    GroupBy
             
           | Update

parseQuery String -> Query