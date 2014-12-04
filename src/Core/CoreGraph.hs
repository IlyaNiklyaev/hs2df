{-# LANGUAGE StandaloneDeriving #-}
module Core.CoreGraph where

import Var
import Literal
import Type
import Core.CoreTools
import Core.CoreTree
import CoreSyn
import Outputable (showSDoc)
import Data.Graph.Analysis.Algorithms.Directed (rootsOf)
import Data.List
import Data.Graph.Inductive
import Data.Tree
import Data.DList (singleton, fromList, toList)
import Control.Monad.RWS
import Control.Arrow

data CalcEntity = CEVar Var | CEExpr Var | CELit Literal | CEDMerge Type | CEPM Var Int | CEError

instance Eq CalcEntity where
        CEVar v1 == CEVar v2 = getVarName v1 == getVarName v2
        CEExpr v1 == CEExpr v2 = getVarName v1 == getVarName v2
        CEExpr v1 == CEVar v2 = getVarName v1 == getVarName v2
        CEVar v1 == CEExpr v2 = getVarName v1 == getVarName v2
        CEPM v1 i == CEPM v2 i2 = and [getVarName v1 == getVarName v2, i == i2]
        CELit l1 == CELit l2 = l1 == l2
        CEDMerge t1 == CEDMerge t2 = (showSDoc $ pprType t1) == (showSDoc $ pprType t2)
        _ == _ = False

instance Ord CalcEntity where
        CEVar v1 `compare` CEVar v2 = getVarName v1 `compare` getVarName v2
        CEExpr v1 `compare` CEExpr v2 = getVarName v1 `compare` getVarName v2
        CEExpr v1 `compare` CEVar v2 = getVarName v1 `compare`getVarName v2
        CEVar v1 `compare` CEExpr v2 = getVarName v1 `compare` getVarName v2
        CEPM v1 i `compare` CEPM v2 i2 = getVarName v1 `compare` getVarName v2
        CELit l1 `compare` CELit l2 = l1 `compare` l2
        CELit _ `compare` _ = LT
        _ `compare` CELit _  = GT
        (CEDMerge t1) `compare` (CEDMerge t2) = (showSDoc $ pprType t1) `compare` (showSDoc $ pprType t2)
        CEDMerge _ `compare` _ = LT
        _ `compare` CEDMerge _  = GT
        _ `compare` _ = EQ
        
instance Show CalcEntity where
        show (CEVar v) = "Name " ++ getVarName v ++ ": " ++ (showSDoc $ pprType $ varType v)
        show (CEPM v i) = "Pattern matching [" ++ show i ++ "]: " ++ getVarName v ++ ": " ++ (showSDoc $ pprType $ varType v)
        show (CEExpr v) = "Expr " ++ getVarName v ++ ": " ++ (showSDoc $ pprType $ varType v)
        show (CELit l) = showLit l
        show (CEDMerge t) = "If"
        show CEError = "Invalid node"

data EdgeRole = Arg Int | Cond | AltHead Int | Alt Int | Default deriving (Show, Eq, Ord)
        
mkCalcEntity :: CoreNode CoreBndr -> CalcEntity
mkCalcEntity (CNName v) = CEVar v
mkCalcEntity (CNPM v i) = CEPM v i
mkCalcEntity (CNExpr (Var v)) = CEExpr v
mkCalcEntity (CNExpr (Lit l)) = CELit l
mkCalcEntity (CNExpr (Case _ _ t _)) = (CEDMerge t)
mkCalcEntity _ = CEError

isVarNode :: LNode CalcEntity -> Bool
isVarNode (_, CEVar _) = True
isVarNode (_, CEExpr _) = True
isVarNode _ = False

mergeDupNodes :: Gr CalcEntity EdgeRole -> Gr CalcEntity EdgeRole
mergeDupNodes gr = delNodes (map fst $ concatMap tail dups) $ insEdges (concatMap (\ (x:xs) -> [ (fst x, o, er) | (er, o) <- concatMap (\ (n, _) -> (\ (_, _, _, adj) -> adj) $ context gr n) xs]) dups) gr where
        dups = filter (\ x -> length x > 1) $ groupBy (\ (_, n1) (_, n2) -> n1 == n2) $ sortBy (\ (_, n1) (_, n2) -> n1 `compare` n2) $ filter isVarNode $ rootsOf gr

getEdgeName :: (Eq m, Variable m) => Int -> Tree (CoreNode m) -> Tree (CoreNode m) -> EdgeRole
getEdgeName ind (Node (CNExpr (Case _ _ _ _)) (_:(Node (CNExpr (Var _)) _):_)) _ = case ind of
        0 -> Cond
        1 -> Default
        _ -> let i = ind - 2 in if i `mod` 2 == 0 then AltHead (i `div` 2) else Alt (i `div` 2)
        
getEdgeName ind (Node (CNExpr (Case _ _ _ _)) _) _ = if ind == 0 then Cond else let i = ind - 1 in if i `mod` 2 == 0 then AltHead (i `div` 2) else Alt (i `div` 2)

getEdgeName ind _ _ = Arg ind

treeToGraph :: (Eq m, Variable m) => Tree (CoreNode m) -> Gr (CoreNode m) EdgeRole
treeToGraph t = uncurry mkGraph . (toList *** toList) . snd $ evalRWS (go t) () [1..]
  where go n@(Node a ns) = do
          i <- state $ head &&& tail
          es <- forM (zip ns [0..]) $ \ (n2,ind) -> do
                j <- go n2
                return (j, i, getEdgeName ind n n2)
          tell (singleton (i, a), fromList es)
          return i