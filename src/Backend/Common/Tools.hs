module Backend.Common.Tools where

import Data.Graph.Inductive
import Type
import Literal
import Var
import Data.Maybe (fromJust)
import Core.CoreGraph
import Backend.Common.Types
import Core.CoreTools
import Data.List (sortBy)

isParamLN ::  Gr CalcEntity EdgeRole -> LNode CalcEntity -> Bool
isParamLN gr (i, ce) = case (ce, context gr i) of
        (CEVar v, ([], _, _, _)) -> not $ isAlgType $ varType v
        (CEExpr v, ([], _, _, _)) -> not $ isAlgType $ varType v
        _ -> False

calcEntityName :: Gr CalcEntity EdgeRole -> LNode CalcEntity -> String
calcEntityName gr n@(i, ce) = case ce of
        (CELit _) -> "lit" ++ show i
        (CEVar v) -> if isParamLN gr n then "var" ++ (getVarName v) else "func" ++ show i
        (CEExpr v) -> if isParamLN gr n then "var" ++ (getVarName v) else "func" ++ show i
        (CEPM _ p) -> "pmatch_" ++ show p ++ "_" ++ show i
        (CEIf _) -> "if" ++ show i
        _ -> "other" ++ show i

calcEntityType :: Gr CalcEntity EdgeRole -> LNode CalcEntity -> Type
calcEntityType gr = snd.calcEntityTypePort gr

calcEntityTypePort :: Gr CalcEntity EdgeRole -> LNode CalcEntity -> TypePort
calcEntityTypePort gr n@(i, ce) = case ce of
        (CELit l) -> ([], literalType l)
        (CEVar v) -> if isParamLN gr n then ([varType v], varType v) else ([], varType v)
        (CEExpr v) -> if isParamLN gr n then ([res], res) else (filter (not.isDictLikeTy) args, res) where (args, res) = splitFunTys $ varType v
        (CEPM v _) -> ([res], head $ filter (not.isDictLikeTy) args) where (args, res) = splitFunTys $ varType v
        (CEIf t) -> (map (calcEntityType gr.(\ (_, x) -> (x, fromJust $ lab gr x))) $ sortBy (\ (r1, _) (r2, _) -> r1 `compare` r2) $ (\ (ins, _, _, _) -> ins) $ context gr i, t)
        --_ -> ([], )

altCount :: Gr CalcEntity EdgeRole -> LNode CalcEntity -> Int
altCount gr n = ((length $ fst $ calcEntityTypePort gr n) `div` 2) - 1

getEdgeType :: Gr CalcEntity EdgeRole -> LEdge EdgeRole -> Type
getEdgeType gr (i, _, _) = snd $ calcEntityTypePort gr (i, fromJust $ lab gr i)