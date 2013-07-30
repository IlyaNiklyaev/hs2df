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
import Backend.OpenCL
import Control.Monad (when)

modifyAST :: Tree (CoreNode CoreBndr) -> Tree (CoreNode CoreBndr)
modifyAST = substLets.elimForAlls.nameApps.foldLambdas.foldApps

modifyGraph :: Gr (CoreNode CoreBndr) EdgeRole -> Gr CalcEntity EdgeRole
modifyGraph = mergeDupNodes.(nmap mkCalcEntity)

setDynFlags :: [DynFlag] -> DynFlags -> DynFlags
setDynFlags fls dflags = foldl dopt_set dflags fls

unsetDynFlags :: [DynFlag] -> DynFlags -> DynFlags
unsetDynFlags fls dflags = foldl dopt_unset dflags fls

backends :: [(String, Gr CalcEntity EdgeRole -> IO ())]
backends = [("-vhdl", genVHDL), ("-graph", genGraphviz), ("-opencl", genOpenCL)]

main :: IO ()
main = do
   res <- defaultErrorHandler defaultLogAction $ runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        _ <- setSessionDynFlags $ setDynFlags [] $ unsetDynFlags [] $ dflags {optLevel = 1, importPaths = ["src"]}
        c <- compileToCoreSimplified "src\\B.hs"
        return $ cm_binds c
   let phase1 = map (modifyAST.toTree.CNBind) res
   args <- getArgs
   when ("-dumpTree" `elem` args) $ putStrLn $ drawForest $ map (fmap show) phase1
   let intBinds = concatMap extractBinds phase1
   let phase2 = intBinds ++ map (bindToAST.splitCaseAlts.deleteCaseBinds) phase1
   let phase3 = modifyGraph.treeToGraph.(substApps phase2).fromJust $ lookupCoreAST phase2 "main"
   when ("-dumpBinds" `elem` args) $ print $ map (\ (x, y, _) -> (x,y)) phase2
   sequence_ $ map ($ phase3) $ map snd $ filter (\ (name, _) -> name `elem` args) backends