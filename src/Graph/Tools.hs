module Graph.Tools where

import Core.CoreGraph
import Data.Graph.Analysis.Algorithms.Directed (rootsOf)
import Data.List
import Var
import Literal
import IdInfo
import OccName (mkVarOcc)
import Name (mkSystemName)
import Unique
import TysPrim
import Data.Graph.Inductive
import Id (isDataConId_maybe)
import Core.CoreTools
import Graph.Types
import Type
import Data.Maybe (fromJust)

isVarNode :: LNode CalcEntity -> Bool
isVarNode (_, CEVar _) = True
isVarNode (_, CEExpr _) = True
isVarNode _ = False

mergeDupNodes :: Gr CalcEntity EdgeRole -> Gr CalcEntity EdgeRole
mergeDupNodes gr = delNodes (map fst $ concatMap tail dups) $ insEdges (concatMap (\ (x:xs) -> [ (fst x, o, er) | (er, o) <- concatMap (\ (n, _) -> (\ (_, _, _, adj) -> adj) $ context gr n) xs]) dups) gr where
        dups = filter (\ x -> length x > 1) $ groupBy (\ (_, n1) (_, n2) -> n1 == n2) $ sortBy (\ (_, n1) (_, n2) -> n1 `compare` n2) $ filter isVarNode $ rootsOf gr

splitConditionalNodes :: Gr CalcEntity EdgeRole -> Gr CalcEntity EdgeRole
splitConditionalNodes gr = delEdges oldCondEdges $ insEdges newCondEdges2 $ insEdges newCondEdges1 $ insNodes condNodes gr
                        where   
                                oldCondEdges = (concatMap (\(_,(dmerge,_)) -> map (\(from,to,_) -> (from,to)) $ filter (\(_,_,l) -> case l of Cond -> True; AltHead _ -> True; _ -> False) $ inn gr dmerge ) dMergePairs)
                                newCondEdges1 = (concatMap (\(cond,(dmerge,_)) -> map (\(from,_,l) -> (from,cond,l)) $ filter (\(_,_,l) -> case l of Cond -> True; AltHead _ -> True; _ -> False) $ inn gr dmerge ) dMergePairs)
                                newCondEdges2 = (map (\(from,(to,_)) -> (from,to,Cond)) dMergePairs)
                                condNodes = (map (\(node,(_,ce)) -> (node, toCondition ce)) dMergePairs)
                                dMergePairs = zip (newNodes 100 gr) dMergeNodes
                                dMergeNodes = filter isDMerge $ labNodes gr
                                isDMerge (_,(CEDMerge _)) = True
                                isDMerge _ = False
                                toCondition _ = CEVar $ mkGlobalVar VanillaId (mkSystemName initTyVarUnique $ mkVarOcc "cond") intPrimTy vanillaIdInfo

isParamLN ::  Gr CalcEntity EdgeRole -> LNode CalcEntity -> Bool
isParamLN gr (i, ce) = case (ce, context gr i) of
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
        (CEVar v) -> splitFunTys $ varType v
        (CEExpr v) -> splitFunTys $ varType v
        (CEPM v _) -> ([res], head $ filter (not.isDictLikeTy) args) where (args, res) = splitFunTys $ varType v
        (CEDMerge t) -> (map (calcEntityType gr.(\ (_, x) -> (x, fromJust $ lab gr x))) $ sortBy (\ (r1, _) (r2, _) -> r1 `compare` r2) $ (\ (ins, _, _, _) -> ins) $ context gr i, t)

altCount :: Gr CalcEntity EdgeRole -> LNode CalcEntity -> Int
altCount gr (n,_) = if hasDefault then argCnt - 2 else argCnt - 1
        where
                argCnt = length $ inn gr n
                hasDefault = not $ null $ filter (\ (_,_,l) -> case l of Default -> True; _ -> False) $ inn gr n