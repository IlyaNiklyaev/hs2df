module Backend.Common.Tools where

import Data.Graph.Inductive
import Type
import Literal
import Var
import Data.Maybe (fromJust)
import Core.CoreGraph
import Backend.Common.Types
import Core.CoreTools
import Core.CoreTypes
import Data.List (sortBy)
import Id (isDataConId_maybe)

isParamLN ::  Gr CalcEntity EdgeRole -> LNode CalcEntity -> Bool
isParamLN gr (i, ce) = case (ce, context gr i) of
        --(CEVar v, ([], _, _, _)) -> case isDataConId_maybe v of Nothing -> True; Just _ -> False
        (CEExpr v, ([], _, _, _)) -> case isDataConId_maybe v of Nothing -> True; Just _ -> False
        _ -> False

calcEntityName :: Gr CalcEntity EdgeRole -> LNode CalcEntity -> String
calcEntityName gr n@(i, ce) = case ce of
        (CELit _) -> "lit" ++ show i
        (CEVar v) -> if isParamLN gr n then "param" ++ (getVarName v) else "func" ++ show i
        (CEExpr v) -> if isParamLN gr n then "param" ++ (getVarName v) else "func" ++ show i
        (CEPM _ p) -> "pmatch_" ++ show p ++ "_" ++ show i
        (CEDMerge _) -> "if" ++ show i
        _ -> "other" ++ show i

calcEntityType :: Gr CalcEntity EdgeRole -> LNode CalcEntity -> Type
calcEntityType gr = snd.calcEntityTypePort gr

calcEntityTypePort :: Gr CalcEntity EdgeRole -> LNode CalcEntity -> TypePort
calcEntityTypePort gr n@(i, ce) = case ce of
        (CELit l) -> ([], literalType l)
        --(CEVar v) -> if isParamLN gr n then ([varType v], varType v) else ([], varType v)
        (CEVar v) -> splitFunTys $ varType v
        --(CEExpr v) -> if isParamLN gr n then ([res], res) else (filter (not.isDictLikeTy) args, res) where (args, res) = splitFunTys $ varType v
        (CEExpr v) -> splitFunTys $ varType v
        (CEPM v _) -> ([res], head $ filter (not.isDictLikeTy) args) where (args, res) = splitFunTys $ varType v
        (CEDMerge t) -> (map (calcEntityType gr.(\ (_, x) -> (x, fromJust $ lab gr x))) $ sortBy (\ (r1, _) (r2, _) -> r1 `compare` r2) $ (\ (ins, _, _, _) -> ins) $ context gr i, t)
        --_ -> ([], )

altCount :: Gr CalcEntity EdgeRole -> LNode CalcEntity -> Int
altCount gr n = if odd argCnt then argCnt `div` 2 else (argCnt `div` 2) - 1 
        where argCnt = (length $ fst $ calcEntityTypePort gr n)

primitivize :: TypePort -> TypePortPrimitive
primitivize (i,o) = (map (\ (num,t) -> primitivizeType ("d" ++ show num) t) $ zip [0,1..] i,primitivizeType "data" o)

getEdgePortMap' :: Gr CalcEntity EdgeRole -> LEdge EdgeRole -> PortMap'
getEdgePortMap' gr (i, j, role) = ((i, cei), (j, cej), case (cej, role) of
        (CELit _, _) -> []
        (_, Arg arg) -> zip iPort $ oPort ("d" ++ show arg)
        (_, Cond) -> zip iPort $ oPort "d0"
        (_, AltHead num) -> zip iPort $ oPort $ "d" ++ show (num + 1)
        (_, Alt num) -> zip iPort $ oPort $ "d" ++ show (aCount + num + 1)
        (_, Default) -> zip iPort $ oPort $ "d" ++ show defAlt
        (CEVar _, _) -> []
        (CEExpr _, _) -> []
        (CEPM v p, _) -> []
        (_, _) -> []
        ) where
                cei = fromJust $ lab gr i
                cej = fromJust $ lab gr j
                (_,oPort') = (calcEntityTypePort gr (i, cei))
                oPort prefix = primitivizeType prefix oPort'
                iPort = oPort "data"
                aCount = altCount gr (j,cej)
                defAlt = (aCount * 2) + 1