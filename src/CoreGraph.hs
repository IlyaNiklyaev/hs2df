module CoreGraph where

import Var
import Literal
import Type
import CoreTools
import CoreTree
import CoreSyn
import Outputable (showSDoc)
import Data.Graph.Analysis.Algorithms.Directed (rootsOf)
import Data.List
import Data.Graph.Inductive

data CalcEntity = CEVar Var | CELit Literal | CEIf | CEError deriving Ord

instance Eq CalcEntity where
        CEVar v1 == CEVar v2 = varName v1 == varName v2
        CELit l1 == CELit l2 = l1 == l2
        CEIf == CEIf = True
        _ == _ = False
        
instance Show CalcEntity where
        show (CEVar v) = getVarName v ++ ": " ++ (showSDoc $ pprType $ varType v)
        show (CELit l) = showLit l
        show CEIf = "If"
        show CEError = "Invalid node"
        
mkCalcEntity :: CoreNode CoreBndr -> CalcEntity
mkCalcEntity (CNName v) = CEVar v
mkCalcEntity (CNExpr (Var v)) = CEVar v
mkCalcEntity (CNExpr (Lit l)) = CELit l
mkCalcEntity (CNExpr (Case _ _ _ _)) = CEIf
mkCalcEntity _ = CEError

mergeDupNodes :: Gr CalcEntity () -> Gr CalcEntity ()
mergeDupNodes gr = delNodes (map fst $ concatMap tail dups) $ insEdges (concatMap (\ (x:xs) -> [ (fst x,o,()) | o <- concatMap (neighbors gr) $ map fst xs]) dups) gr where dups = filter (\ x -> length x > 1) $ groupBy (\ (_,n1) (_, n2) -> n1 == n2) $ sortBy (\ (_, n1) (_, n2) -> n1 `compare` n2)  $ rootsOf gr