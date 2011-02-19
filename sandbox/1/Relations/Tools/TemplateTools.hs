{-# LANGUAGE TemplateHaskell #-}
module Tools.TemplateTools  where

import Language.Haskell.TH.Syntax

mkVars :: String -> [Name]
mkVars name = map (mkName . (name++) . show) [1..]

as, outps :: [Name]
as    = mkVars "a"
outps = mkVars "outp"

oneTupleWrap :: Type -> Type
oneTupleWrap = AppT $ ConT oneTuple

oneTuple :: Name
oneTuple = mkName "OneTuple"

cont :: String -> Type
cont = ConT . mkName

-- makes tuple as is, without OneTuple Constructor
mkTupleType' :: [Type] -> Type
mkTupleType' types = foldl f (TupleT (length types)) types
    where f acc el = AppT acc el

-- makes tuple using OneTuple constructor as tuple singleton
mkTupleType :: [Type] -> Type
mkTupleType []	     = TupleT 0
mkTupleType [_type] = oneTupleWrap _type
mkTupleType types   = foldl f (TupleT (length types)) types
    where f acc el = AppT acc el

-- applies type sequentially
typeApp :: [Type] -> Type
typeApp (t:ts) = foldl f t ts
    where f acc el = AppT acc el

-- makes tuple using OneTuple constructor as tuple singleton
mkTupP :: [Name] -> Pat
mkTupP []  = TupP []
mkTupP [x] = ConP oneTuple [VarP x]
mkTupP xs  = TupP $ map VarP xs

mkTupE :: [Exp] -> Exp
mkTupE []  = TupE []
mkTupE [x] = AppE (ConE $ oneTuple) x
mkTupE xs  = TupE xs

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
       where _type 	 = instanceHead "Timable" [insOf]
             insOf 	 = mkTupleType' (cont "UTCTime" : map VarT (tail vars))
             vars 	 = take n as
             toTime  	 = mkName "toTime"
             clauses 	 = [Clause [TupP $ map VarP vars]
                               (NormalB $ VarE $ vars !! 0)
                               []
                           ]

-- apply each function in input tuple to input in order to produce output tuple
tapplyInstances :: Int -> Q [Dec]
tapplyInstances n = return $ map tapplyInstance [1 .. n]

tapplyInstance :: Int -> Dec
tapplyInstance n = InstanceD [] _type
                       [FunD tapply clauses]
    where _type  = instanceHead "TupleApply" [mkTupleType fnsT, inpT, mkTupleType outpsT]
          clauses = [Clause [mkTupP fns, VarP inp]
                            (NormalB (mkTupE . map (flip AppE $ VarE inp) $ fnsE))
                            []
                    ]
          tapply = mkName "tapply"
          fns    = take n $ mkVars "fn"
          inp    = mkName "inp"

          outpsT = map VarT . take n $ outps
          fnsT   = map (AppT (AppT ArrowT inpT)) outpsT
          inpT   = VarT inp
          fnsE   = map VarE fns

-- concat 2 tuple
tconcatInstances :: Int -> Q [Dec]
tconcatInstances x = return $ map tconcatInstance [(n, m) | n <- [0..x], m <- [0..x]]

tconcatInstance :: (Int, Int) -> Dec
tconcatInstance (n, m) = InstanceD [] _type
                           [FunD tconcat clauses]
    where _type   = instanceHead "TupleConcat" $ map (mkTupleType . map VarT) [vars1, vars2, vars3]
          clauses = [Clause [mkTupP vars1, mkTupP vars2]
                            (NormalB (mkTupE' vars3))
                            []
                    ]

          vars1   = take n as
          vars2   = take m . drop n $ as
          vars3   = vars1 ++ vars2
          tconcat = mkName "tconcat"

