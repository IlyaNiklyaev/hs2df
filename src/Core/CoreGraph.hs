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
import Data.Maybe (fromJust)

data CalcEntity = CEVar Var | CEExpr Var | CELit Literal | CEIf | CEPM Var Int | CEError deriving Ord

instance Eq CalcEntity where
        CEVar v1 == CEVar v2 = varName v1 == varName v2
        CEExpr v1 == CEExpr v2 = varName v1 == varName v2
        CEExpr v1 == CEVar v2 = varName v1 == varName v2
        CEVar v1 == CEExpr v2 = varName v1 == varName v2
        CEPM v1 i == CEPM v2 i2 = and [varName v1 == varName v2, i == i2]
        CELit l1 == CELit l2 = l1 == l2
        CEIf == CEIf = True
        _ == _ = False
        
instance Show CalcEntity where
        show (CEVar v) = "Name " ++ getVarName v ++ ": " ++ (showSDoc $ pprType $ varType v)
        show (CEPM v i) = "Patter matching [" ++ show i ++ "]: " ++ getVarName v ++ ": " ++ (showSDoc $ pprType $ varType v)
        show (CEExpr v) = "Expr " ++ getVarName v ++ ": " ++ (showSDoc $ pprType $ varType v)
        show (CELit l) = showLit l
        show CEIf = "If"
        show CEError = "Invalid node"

data EdgeRole = ER String

instance Show EdgeRole where
        show (ER s) = s
        
mkCalcEntity :: CoreNode CoreBndr -> CalcEntity
mkCalcEntity (CNName v) = CEVar v
mkCalcEntity (CNPM v i) = CEPM v i
mkCalcEntity (CNExpr (Var v)) = CEExpr v
mkCalcEntity (CNExpr (Lit l)) = CELit l
mkCalcEntity (CNExpr (Case _ _ _ _)) = CEIf
mkCalcEntity _ = CEError

mergeDupNodes :: Gr CalcEntity EdgeRole -> Gr CalcEntity EdgeRole
mergeDupNodes gr = delNodes (map fst $ concatMap tail dups) $ insEdges (concatMap (\ (x:xs) -> [ (fst x,o,ER "data") | o <- concatMap (neighbors gr) $ map fst xs]) dups) gr where dups = filter (\ x -> length x > 1) $ groupBy (\ (_,n1) (_, n2) -> n1 == n2) $ sortBy (\ (_, n1) (_, n2) -> n1 `compare` n2)  $ rootsOf gr

getAltNumber :: (Eq m, Variable m) => Tree (CoreNode m) -> [Tree (CoreNode m)] -> Maybe EdgeRole
getAltNumber node alts = case findIndex (== node) alts of
        Just i -> if i `mod` 2 == 0 then Just (ER ("C" ++ show (i `div` 2))) else Just (ER ("F" ++ show (i `div` 2)))
        Nothing -> Nothing

getEdgeName :: (Eq m, Variable m) => Tree (CoreNode m) -> Tree (CoreNode m) -> EdgeRole
getEdgeName (Node (CNExpr (Case _ _ _ _)) (cond:def@(Node (CNExpr (Var _)) _):alts)) node
        | def == node = ER "default"
        | cond == node = ER "cond"
        | otherwise = case getAltNumber node alts of
                Just er -> er
                Nothing -> ER ""
getEdgeName (Node (CNExpr (Case _ _ _ _)) (cond:alts)) node
        | cond == node = ER "cond"
        | otherwise = case getAltNumber node alts of
                Just er -> er
                Nothing -> ER ""
getEdgeName (Node (CNExpr (Var _)) args) node = case findIndex (== node) args of
                Just i ->  ER ("data" ++ show i)
                Nothing -> ER "data"
getEdgeName (Node (CNName _) args) node = case findIndex (== node) args of
                Just i ->  ER ("data" ++ show i)
                Nothing -> ER "data"
getEdgeName (Node _ _) _ = ER "data"

treeToGraph :: (Eq m, Variable m) => Tree (CoreNode m) -> Gr (CoreNode m) EdgeRole
treeToGraph t = uncurry mkGraph . (toList *** toList) . snd $ evalRWS (go t) () [1..]
  where go n@(Node a ns) = do
          i <- state $ head &&& tail
          es <- forM ns $ \ n2 -> do
                j <- go n2
                return (j, i, getEdgeName n n2)
          tell (singleton (i, a), fromList es)
          return i