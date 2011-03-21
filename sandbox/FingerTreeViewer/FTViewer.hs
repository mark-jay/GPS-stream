{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module FTViewer where

import FingerTree

import Control.Arrow((&&&), first)

import qualified Data.Char as Char
import Data.Monoid
import qualified Data.List as List

--------------------------------------------------------------------------------
-- my code

someFT :: FingerTree (Sum Int) Int
someFT = fromList [0..10]

someFT100 :: FingerTree (Sum Int) Int
someFT100 = fromList [0..100]

anotherFT :: FingerTree (Sum Int) Int
anotherFT = fromList [0..1000]

ftConcat = anotherFT >< someFT

someFT1 :: (FingerTree (Sum Int) Int, FingerTree (Sum Int) Int)
someFT1 = split (> Sum 4) someFT

instance Measured (Sum Int) Int where
    measure = Sum    

----------------------------------------
-- viewing

pprint :: (PrettyShow a) => a -> IO ()
pprint t = putStrLn (pshow t 0 toIndent)

toIndent :: Int -> [Char]
toIndent n = concat $ replicate n "  "

----------------------------------------
-- PrettyShow
class PrettyShow a where
    pshow :: a -> Int -> (Int -> String) -> String
    -- ^ object -> indentDepth -> indenter -> output

instance PrettyShow Int where
    pshow x n f = f n ++ show x

instance (Show a) => PrettyShow (Sum a) where
    pshow x n f = f n ++ show x

instance (PrettyShow v, PrettyShow a) => PrettyShow (FingerTree v a) where
    pshow = showTree

showTree :: (PrettyShow a) => FingerTree v a -> Int -> (Int -> String) -> String
showTree Empty n f		= f n ++ "Empty" ++ "\n"
showTree (Single a) n f		= f n ++ "Single " ++ "\n" ++ pshow a (n+1) f
showTree (Deep _ d1 t d2) n f	= f n ++ "Deep :\n" ++
                                  pshow d1 (n+1) f ++ "\n" ++
                                  showTree t (n+1) f ++ 
                                  pshow d2 (n+1) f ++ "\n"

instance (PrettyShow a) => PrettyShow (Node v a) where
    pshow = showNode

pShowHelper :: (PrettyShow a) => [a] -> Int -> (Int -> String) -> String
pShowHelper xs n f = List.intercalate "\n" $ map (\x -> pshow x n f) xs

showNode :: (PrettyShow a) => Node v a -> Int -> (Int -> String) -> String
showNode (Node2 _ a1 a2) n f    = f n ++ "Node2 :\n" ++ 
                                  pShowHelper [a1, a2] (n+1) f ++ "\n"
showNode (Node3 _ a1 a2 a3) n f = f n ++ "Node3 :\n" ++ 
                                  pShowHelper [a1, a2, a3] (n+1) f ++ "\n"

instance (PrettyShow a) => PrettyShow (Digit a) where
    pshow = showDigit

showDigit
  :: (PrettyShow t) => Digit t -> Int -> (Int -> [Char]) -> [Char]
showDigit (One a1) n f 		= f n ++ "One:\n" ++
                                  pShowHelper [a1] (n+1) f
showDigit (Two a1 a2) n f 	= f n ++ "Two:\n" ++
                                  pShowHelper [a1, a2] (n+1) f
showDigit (Three a1 a2 a3) n f 	= f n ++ "Three:\n" ++
                                  pShowHelper [a1, a2, a3] (n+1) f
showDigit (Four a1 a2 a3 a4) n f= f n ++ "Four:\n" ++
                                  pShowHelper [a1, a2, a3, a4] (n+1) f

----------------------------------------
-- game

prompt = "enter a command:"

game :: IO ()
game = game' (parseAction "NEW" undefined)

game' :: FingerTree (Sum Int) Int -> IO ()
game' finTree = do
  pprint finTree
  putStrLn prompt
  a <- getLine
  let newTree = parseAction a finTree
  game' newTree

parseAction :: String
            -> (FingerTree (Sum Int) Int -> 
                FingerTree (Sum Int) Int)
parseAction args = mapper
    where (cmd, args') = (first $ map Char.toUpper) $ break (==' ') args
                      -- add elems left/right
          mapper tree | cmd == "L"   = foldr (<|) tree $ map read $ words args'
                      | cmd == "R"   = foldl (|>) tree $ map read $ words args'
                      -- concat seq left/right
                      | cmd == "RS"  = tree >< fromList (read args'::[Int])
                      | cmd == "LS"  = fromList (read args'::[Int]) >< tree
                      -- new empty seq
                      | cmd == "NEW" = fromList []
                      -- remove elem left/right
                      | cmd == "LR"  = remL tree
                      | cmd == "RR"  = remR tree
                      -- do nothing
                      | otherwise    = tree

remL tree = toTree . viewl $ tree
    where toTree EmptyL 	= tree
          toTree (_ :< tree')	= tree'

remR tree = toTree . viewr $ tree
    where toTree EmptyR 	= tree
          toTree (tree' :> _)	= tree'

{-
data FingerTree v a
	= Empty
	| Single a
	| Deep !v !(Digit a) (FingerTree v (Node v a)) !(Digit a)
data Node v a = Node2 !v a a | Node3 !v a a a
	deriving Show
data Digit a
    = One a
    | Two a a
    | Three a a a
    | Four a a a a
    deriving Show

-}