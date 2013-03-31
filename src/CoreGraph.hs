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
import Data.Tree
import Data.DList (singleton, fromList, toList)
import Control.Monad.RWS
import Control.Arrow

data CalcEntity = CEVar Var | CEExpr Var | CELit Literal | CEIf | CEError deriving Ord

instance Eq CalcEntity where
        CEVar v1 == CEVar v2 = varName v1 == varName v2
        CEExpr v1 == CEExpr v2 = varName v1 == varName v2
        CEExpr v1 == CEVar v2 = varName v1 == varName v2
        CEVar v1 == CEExpr v2 = varName v1 == varName v2
        CELit l1 == CELit l2 = l1 == l2
        CEIf == CEIf = True
        _ == _ = False
        
instance Show CalcEntity where
        show (CEVar v) = "Name " ++ getVarName v ++ ": " ++ (showSDoc $ pprType $ varType v)
        show (CEExpr v) = "Expr " ++ getVarName v ++ ": " ++ (showSDoc $ pprType $ varType v)
        show (CELit l) = showLit l
        show CEIf = "If"
        show CEError = "Invalid node"

data EdgeRole = EP String deriving Show
        
mkCalcEntity :: CoreNode CoreBndr -> CalcEntity
mkCalcEntity (CNName v) = CEVar v
mkCalcEntity (CNExpr (Var v)) = CEExpr v
mkCalcEntity (CNExpr (Lit l)) = CELit l
mkCalcEntity (CNExpr (Case _ _ _ _)) = CEIf
mkCalcEntity _ = CEError

mergeDupNodes :: Gr CalcEntity () -> Gr CalcEntity ()
mergeDupNodes gr = delNodes (map fst $ concatMap tail dups) $ insEdges (concatMap (\ (x:xs) -> [ (fst x,o,()) | o <- concatMap (neighbors gr) $ map fst xs]) dups) gr where dups = filter (\ x -> length x > 1) $ groupBy (\ (_,n1) (_, n2) -> n1 == n2) $ sortBy (\ (_, n1) (_, n2) -> n1 `compare` n2)  $ rootsOf gr

getEdgeName :: Tree (CoreNode m) -> Tree (CoreNode m) -> EdgeRole
getEdgeName (Node (CNExpr (Case expr b t alts)) _) _ = EP "alt"
getEdgeName (Node _ _) _ = EP "data"

treeToGraph :: Tree (CoreNode m) -> Gr (CoreNode m) EdgeRole
treeToGraph t = uncurry mkGraph . (toList *** toList) . snd $ evalRWS (go t) () [1..]
  where go n@(Node a ns) = do
          i <- state $ head &&& tail
          es <- forM ns $ \ n2 -> do
                j <- go n2
                return (j, i, getEdgeName n n2)
          tell (singleton (i, a), fromList es)
          return i