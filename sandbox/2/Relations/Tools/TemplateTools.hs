{-# LANGUAGE TemplateHaskell, PatternGuards #-}
module Tools.TemplateTools ( timableInstances
                           , tapplyInstances
                           , tconcatInstances
                           )
    where

import Language.Haskell.TH.Syntax

isLeft (Left _) = True
isLeft _	= False

mkVars :: String -> [Name]
mkVars name = mkVarsByList name [1..]

mkVarsByList :: (Show a) => String -> [a] -> [Name]
mkVarsByList name = map (mkName . (name++) . show) 

mkVarsByList' :: (Show a) => String -> [a] -> [String]
mkVarsByList' name = map ((name++) . show) 

as, outps :: [Name]
as         = mkVars "a"
outps      = mkVars "outp"

oneTuple :: Name
oneTuple = mkName "OneTuple"

oneTupleWrap :: Type -> Type
oneTupleWrap = AppT $ ConT oneTuple

cont :: String -> Type
cont = ConT . mkName

-- makes tuple as is, without OneTuple Constructor
mkTupleType' :: [Type] -> Type
mkTupleType' types = foldl f (TupleT (length types)) types
    where f acc el = AppT acc el

-- makes tuple using OneTuple constructor as tuple singleton
mkTupleType :: [Type] -> Type
mkTupleType [] = TupleT 0
mkTupleType [_type] = oneTupleWrap _type
mkTupleType types = mkTupleType' types

-- like mkTupleType
mkFunType :: [Type] -> Type
mkFunType = foldl1 f
    where f acc el = AppT (AppT ArrowT acc) el

-- applies type sequentially
typeApp :: [Type] -> Type
typeApp (t:ts) = foldl f t ts
    where f acc el = AppT acc el

-- makes tuple using OneTuple constructor as tuple singleton
mkTupP :: [Name] -> Pat
mkTupP [] = TupP []
mkTupP [x] = ConP oneTuple [VarP x]
mkTupP xs = TupP $ map VarP xs

mkTupE :: [Exp] -> Exp
mkTupE [] = TupE []
mkTupE [x] = AppE (ConE $ oneTuple) x
mkTupE xs = TupE xs

mkTupE' :: [Name] -> Exp
mkTupE' = mkTupE . map VarE

instanceHead :: String -> [Type] -> Type
instanceHead name types = typeApp (cont name : types)
-- ---------------------------------------------------------------------------
-- instances generators

-- Generates declarations for tuples, where first elem has type UTCTime
timableInstances :: Int -> Q [Dec]
timableInstances n = return $ map timableInstance [1..n]

timableInstance :: Int -> Dec
timableInstance n = InstanceD [] _type
                       [FunD toTime clauses]
       where _type = instanceHead "Timable" [insOf]
             insOf = mkTupleType' (cont "UTCTime" : map VarT (tail vars))
             vars = take n as
             toTime = mkName "toTime"
             clauses = [Clause [TupP $ map VarP vars]
                               (NormalB $ VarE $ vars !! 0)
                               []
                           ]

-- apply each function in input tuple to input in order to produce output tuple
tapplyInstances :: Int -> Q [Dec]
tapplyInstances n = return $ map tapplyInstance [1 .. n]

tapplyInstance :: Int -> Dec
tapplyInstance n = InstanceD [] _type
                       [FunD tapply clauses]
    where _type = instanceHead "TupleApply" [mkTupleType fnsT, inpT, mkTupleType outpsT]
          clauses = [Clause [mkTupP fns, VarP inp]
                            (NormalB (mkTupE . map (flip AppE $ VarE inp) $ fnsE))
                            []
                    ]
          tapply = mkName "tapply"
          fns = take n $ mkVars "fn"
          inp = mkName "inp"

          outpsT = map VarT . take n $ outps
          fnsT = map (AppT (AppT ArrowT inpT)) outpsT
          inpT = VarT inp
          fnsE = map VarE fns

-- concat 2 tuple
tconcatInstances :: Int -> Q [Dec]
tconcatInstances x = return $ map tconcatInstance [(n, m) | n <- [0..x], m <- [0..x]]

tconcatInstance :: (Int, Int) -> Dec
tconcatInstance (n, m) = InstanceD [] _type
                           [FunD tconcat clauses]
    where _type = instanceHead "TupleConcat" $ map (mkTupleType . map VarT) [vars1, vars2, vars3]
          clauses = [Clause [mkTupP vars1, mkTupP vars2]
                            (NormalB (mkTupE' vars3))
                            []
                    ]

          vars1 = take n as
          vars2 = take m . drop n $ as
          vars3 = vars1 ++ vars2
          tconcat = mkName "tconcat"


{-
Can't compile smth like:
test :: (a -> a) -> b -> c -> (b, c)
test fn a b = (fn a, fn b)
so we don't need next functions


-- map by selector:
--  ([1,2], [1,2]) -> (f a1 a2 -> g a1 a2)
tmapConstructorInstances :: ([Int], Either [Int] Int) -> Int -> Q [Dec]
tmapConstructorInstances (args1, args2) tupleSize = 
    return [ tmapConstructorInstance (args1, args2) t | t <- [0 .. tupleSize]]

tmapConstructorInstance :: ([Int], Either [Int] Int) -> Int -> Dec
tmapConstructorInstance (args1, args2) tupleSize = 
    InstanceD [] _type
        [FunD tmap clauses]
    where _type   = instanceHead "TupleMap" $ [fnType, tupT1, tupT2]
          clauses = [Clause [fnP, inpTupP] body []]
          tmap    = mkName "tmap"

          -- type generating
          mkFnType inpT outpT = mkFunType [ typeApp $ map VarT inpT
                                          , typeApp $ map VarT outpT
                                          ]

          fnType   :: Type
          fnType | isLeft args2  = mkFnType (mkName "f" : as1) (mkName "g" : as2)
                 | otherwise	 = mkFnType (mkName "f" : as1) as2

          tupT1   :: Type
          tupT1   = mkTupT ["f"] bs1
          tupT2   :: Type
          tupT2 | isLeft args2 = mkTupT ["g"] bs2
                | otherwise    = mkTupT []    bs2

          as1 	  = mkVarsByList "a" args1
          bs1	  = mkVarsByList' "b" args1

          (as2, bs2) = let a2 = either id (:[]) args2
                       in (mkVarsByList "a" a2, mkVarsByList' "b" a2)

          mkTupT :: [String] -> [String] -> Type
          mkTupT cs vs = mkTupleType $ map typeApp $ mkTup cs vs

          mkTup cs vs = map (map (VarT . mkName)) ts
              where ts = [ (cs ++ map id vs) 
                               | tnum <- [1 .. tupleSize] ]

          -- body generating
          vars = [ (mkName $ "a" ++ show tnum)  | tnum <- [1 .. tupleSize] ]

          fnP :: Pat
          fnP = VarP $ mkName $ "fn"

          inpTupP :: Pat
          inpTupP = mkTupP vars

          body :: Body                    
          body = NormalB . mkTupE . map (AppE (VarE $ mkName "fn") . VarE) $ vars


{-
instance TupleMap (f a1 a2 -> g a1 a2) 	(f b1_1 b2_1, f b1_2 b2_2) (g b1_1 b1_2, g b2_1 b2_2) where
instance TupleMap (f a1 a2 -> g a1) 	(f b1 b2, f c) (g b, g c) where
instance TupleMap (f a1 a2 -> g a2) 	(f b, f c) (g b, g c) where
instance TupleMap (f a1 -> g a1) 	(f b, f c) (g b, g c) where
instance TupleMap (f a1 -> a1) 		(f b, f c) (g b, g c) where
-}
-}
