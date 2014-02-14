module Backend.OpenCL.Tools where

import Data.Graph.Inductive
import Type
import Literal
import Var
import Data.Maybe (fromJust)
import Core.CoreGraph
import Backend.OpenCL.Types
import Backend.OpenCL.BuiltIn.Types
import Backend.OpenCL.BuiltIn.Functions
import Core.CoreTools
import Data.List (sortBy, elemIndex)

isParamLN ::  Gr CalcEntity EdgeRole -> LNode CalcEntity -> Bool
isParamLN gr (i, ce) = case (ce, context gr i) of
        (CEVar v, ([], _, _, _)) -> null $ getFuncBody v
        (CEExpr v, ([], _, _, _)) -> null $ getFuncBody v
        _ -> False

calcEntityName :: Gr CalcEntity EdgeRole -> LNode CalcEntity -> String
calcEntityName gr n@(i, ce) = case ce of
        (CELit l) -> "lit" ++ show i
        (CEVar v) -> if isParamLN gr n then "var" ++ (getVarName v) else "func" ++ show i
        (CEExpr v) -> if isParamLN gr n then "var" ++ (getVarName v) else "func" ++ show i
        (CEPM v p) -> "pmatch_" ++ show p ++ "_" ++ show i
        (CEIf _) -> "if" ++ show i
        _ -> "other" ++ show i

calcEntityTypeIface :: Gr CalcEntity EdgeRole -> LNode CalcEntity -> TypeIface
calcEntityTypeIface gr = getTypeIface'.calcEntityType gr

calcEntityType :: Gr CalcEntity EdgeRole -> LNode CalcEntity -> Type
calcEntityType gr = snd.calcEntityTypePort gr

emptyTypeIface :: TypeIface
emptyTypeIface = TypeIface {sType = ""}

fromTypePort :: TypePort -> Port
fromTypePort (iTypes, oType) = ("data", oBusType) : (concatMap (\ (t, i) -> [
                ("d" ++ show i, sType t)
                ]) $ zip iType [0,1..]) where
                oBusType = (sType.getTypeIface') oType
                iType = map (getTypeIface') iTypes

calcEntityTypePort :: Gr CalcEntity EdgeRole -> LNode CalcEntity -> TypePort
calcEntityTypePort gr n@(i, ce) = case ce of
        (CELit l) -> ([], literalType l)
        (CEVar v) -> if isParamLN gr n then ([varType v], varType v) else ([], varType v)
        (CEExpr v) -> if isParamLN gr n then ([res], res) else (filter (not.isDictLikeTy) args, res) where (args, res) = splitFunTys $ varType v
        (CEPM v p) -> ([res], head $ filter (not.isDictLikeTy) args) where (args, res) = splitFunTys $ varType v
        (CEIf t) -> (map (calcEntityType gr.(\ (_, x) -> (x, fromJust $ lab gr x))) $ sortBy (\ (r1, _) (r2, _) -> r1 `compare` r2) $ (\ (ins, _, _, _) -> ins) $ context gr i, t)
        --_ -> ([], )

calcEntityPort :: Gr CalcEntity EdgeRole -> LNode CalcEntity -> Port
calcEntityPort gr n = fromTypePort $ calcEntityTypePort gr n

altCount :: Gr CalcEntity EdgeRole -> LNode CalcEntity -> Int
altCount gr n = ((length $ calcEntityPort gr n) `div` 2) - 1

getEdgePortMap :: Gr CalcEntity EdgeRole -> LEdge EdgeRole -> PortMap
getEdgePortMap gr e@(i, j, role) = ((i, cei), (j, cej), case (cej, role) of
        (CELit _, _) -> []
        (_, Arg arg) -> map (\ ((x,y),z) -> (x,z,y)) $ zip oPort $ argPort arg
        (_, Cond) -> map (\ ((x,y),z) -> (x,z,y)) $ zip oPort $ cond
        (_, AltHead num) -> map (\ ((x,y),z) -> (x,z,y)) $ zip oPort $ altHead num
        (_, Alt num) -> map (\ ((x,y),z) -> (x,z,y)) $ zip oPort $ alt num
        (_, Default) -> map (\ ((x,y),z) -> (x,z,y)) $ zip oPort $ def
        (CEVar _, _) -> []
        (CEExpr _, _) -> []
        (CEPM v p, _) -> []
        (_, _) -> []
        ) where
                cei = fromJust $ lab gr i
                cej = fromJust $ lab gr j
                --oPort = take 1  $ drop (fromJust $ elemIndex e $ out gr i) (calcEntityPort gr (i, cei))
                oPort = take 1 $ (calcEntityPort gr (i, cei))
                argPort arg = map fst $ take 1 $ drop (1 * (arg + 1)) (calcEntityPort gr (j, cej))
                cond = map fst $ take 1 $ drop 1 (calcEntityPort gr (j, cej))
                def = map fst $ reverse $ take 1 $ reverse (calcEntityPort gr (j, cej))
                altHead num = map fst $ take 1 $ drop (num + 2) (calcEntityPort gr (j, cej))
                alt num = map fst $ take 1 $ drop (num + altCount gr (j, cej) + 2) (calcEntityPort gr (j, cej))