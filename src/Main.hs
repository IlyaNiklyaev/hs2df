{-# LANGUAGE StandaloneDeriving, TypeSynonymInstances, FlexibleInstances #-}
module Main where

import GHC 
import GHC.Paths ( libdir ) 
import DynFlags
import System.Environment

import CoreTree
import Data.Tree
import Data.Graph.Inductive
import Data.Maybe (fromJust)
import CoreSyn (CoreBndr)
import GhcMonad (liftIO)
import CoreGraph
import Backend.Graphviz

modifyAST :: Tree (CoreNode m) -> Tree (CoreNode m)
modifyAST = elimForAlls.nameApps.foldLambdas.foldApps

modifyGraph :: Gr (CoreNode CoreBndr) EdgeRole -> Gr CalcEntity EdgeRole
modifyGraph = (nmap mkCalcEntity)

setDynFlags :: [DynFlag] -> DynFlags -> DynFlags
setDynFlags fls dflags = foldl dopt_set dflags fls

unsetDynFlags :: [DynFlag] -> DynFlags -> DynFlags
unsetDynFlags fls dflags = foldl dopt_unset dflags fls

main :: IO ()
main = do
   _ <- getArgs
   res <- defaultErrorHandler defaultLogAction $ runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        _ <- setSessionDynFlags $ setDynFlags [] $ unsetDynFlags [] $ dflags {optLevel = 1, importPaths = ["src"]}
        --dflags <- getSessionDynFlags
        --liftIO $ handleFlagWarnings dflags $ map (noLoc.show) $ filter (\ x -> dopt x dflags) [Opt_D_dump_cmm .. Opt_PackageTrust]
        c <- compileToCoreSimplified "src\\B.hs"
        return $ cm_binds c
   let phase1 = map (modifyAST.toTree.CNBind) res
   let intBinds = concatMap extractBinds phase1
   let phase2 = intBinds ++ map (bindToAST.splitCaseAlts.deleteCaseBinds.deleteLets) phase1
   --print $ map (\ (x,y,_) -> (x,y)) phase2
   let phase3 = modifyGraph.treeToGraph.(substApps phase2).fromJust $ lookupCoreAST phase2 "main"
   --let phase3 = modifyGraph.treeToGraph.fromJust $ lookupCoreAST phase2 "main"
   genGraphviz phase3