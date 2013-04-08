{-# LANGUAGE StandaloneDeriving, TypeSynonymInstances, FlexibleInstances #-}
module Main where

import GHC 
import GHC.Paths ( libdir ) 
import DynFlags
import System.Environment

import Core.CoreTree
import Data.Tree
import Data.Graph.Inductive
import Data.Maybe (fromJust)
import CoreSyn (CoreBndr)
import Core.CoreGraph
import Backend.Graphviz
import Backend.VHDL.VHDL

modifyAST :: Tree (CoreNode m) -> Tree (CoreNode m)
modifyAST = elimForAlls.nameApps.foldLambdas.foldApps

modifyGraph :: Gr (CoreNode CoreBndr) EdgeRole -> Gr CalcEntity EdgeRole
modifyGraph = mergeDupNodes.(nmap mkCalcEntity)

setDynFlags :: [DynFlag] -> DynFlags -> DynFlags
setDynFlags fls dflags = foldl dopt_set dflags fls

unsetDynFlags :: [DynFlag] -> DynFlags -> DynFlags
unsetDynFlags fls dflags = foldl dopt_unset dflags fls

backends :: [(String, Gr CalcEntity EdgeRole -> IO ())]
backends = [("-vhdl", genVHDL), ("-graph", genGraphviz)]

main :: IO ()
main = do
   res <- defaultErrorHandler defaultLogAction $ runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        _ <- setSessionDynFlags $ setDynFlags [] $ unsetDynFlags [] $ dflags {optLevel = 1, importPaths = ["src"]}
        c <- compileToCoreSimplified "src\\B.hs"
        return $ cm_binds c
   let phase1 = map (modifyAST.toTree.CNBind) res
   let intBinds = concatMap extractBinds phase1
   let phase2 = intBinds ++ map (bindToAST.splitCaseAlts.deleteCaseBinds.deleteLets) phase1
   let phase3 = modifyGraph.treeToGraph.(substApps phase2).fromJust $ lookupCoreAST phase2 "main"
   args <- getArgs
   sequence_ $ map ($ phase3) $ map snd $ filter (\ (name, fun) -> name `elem` args) backends